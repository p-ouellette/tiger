structure Semant :
  sig val transProg : Absyn.exp -> unit end =
struct

  structure A = Absyn
  structure E = Env
  structure S = Symbol
  structure T = Types
  structure Tr = Translate

  val error = ErrorMsg.error
  val impossible = ErrorMsg.impossible

  fun tyMatches (T.NIL, T.RECORD _) = true
    | tyMatches (_, T.NIL)          = impossible "nil declared type"
    | tyMatches (T.ERROR, _)        = true
    | tyMatches (_, T.ERROR)        = true
    | tyMatches (ty, declared)      = ty = declared

  local
    fun checkArith (T.INT, T.INT, _) = T.INT
      | checkArith (T.ERROR, _, _)   = T.INT
      | checkArith (_, T.ERROR, _)   = T.INT
      | checkArith (_, _, pos) = (error pos "integer required"; T.INT)

    fun checkEq (T.NIL, T.RECORD _, _) = T.INT
      | checkEq (T.RECORD _, T.NIL, _) = T.INT
      | checkEq (T.ERROR, _, _)        = T.INT
      | checkEq (_, T.ERROR, _)        = T.INT
      | checkEq (T.NIL, T.NIL, pos) =
          (error pos "cannot determine type of nil"; T.INT)
      | checkEq (l, r, pos) =
          (if l <> r then error pos "type mismatch" else (); T.INT)

    fun checkOrd (T.INT, T.INT, _)       = T.INT
      | checkOrd (T.STRING, T.STRING, _) = T.INT
      | checkOrd (T.ERROR, _, _)         = T.INT
      | checkOrd (_, T.ERROR, _)         = T.INT
      | checkOrd (_, _, pos) = (error pos "invalid comparison"; T.INT)
  in
    fun checkOp A.PlusOp   = checkArith
      | checkOp A.MinusOp  = checkArith
      | checkOp A.TimesOp  = checkArith
      | checkOp A.DivideOp = checkArith
      | checkOp A.EqOp     = checkEq
      | checkOp A.NeqOp    = checkEq
      | checkOp A.LtOp     = checkOrd
      | checkOp A.LeOp     = checkOrd
      | checkOp A.GtOp     = checkOrd
      | checkOp A.GeOp     = checkOrd
  end

  fun actualTy (T.NAME(_,ref(SOME ty))) = actualTy ty
    | actualTy (T.NAME _) = impossible "incomplete name type"
    | actualTy ty = ty

  fun lookupNameTy (tenv, id, pos) =
        (case S.look(tenv, id)
           of SOME ty => ty
            | NONE => (error pos ("undefined type " ^ S.name id); T.ERROR))

  val lookupTy = actualTy o lookupNameTy

  fun transExp (venv, tenv, level, attrs) = let
        fun trexp (A.VarExp var)  = trvar var
          | trexp (A.NilExp)      = {exp=(), ty=T.NIL}
          | trexp (A.IntExp _)    = {exp=(), ty=T.INT}
          | trexp (A.StringExp _) = {exp=(), ty=T.STRING}
          | trexp (A.CallExp{func,args,pos}) =
              (case S.look(venv, func)
                 of SOME(E.FunEntry{formals,result,...}) => let
                      fun checkArg(arg, ty, argn) =
                            (if not(tyMatches(#ty(trexp arg), ty)) then
                               error pos ("bad argument #" ^ Int.toString argn ^
                                          " to function " ^ S.name func ^
                                          " (type mismatch)")
                             else (); argn + 1)
                      in
                        if length args <> length formals then
                          error pos ("wrong number of arguments to function " ^
                                     S.name func)
                        else ();
                        ListPair.foldl checkArg 1 (args, formals);
                        {exp=(), ty=result}
                      end
                  | _ => (error pos ("undefined function " ^ S.name func);
                          {exp=(), ty=T.ERROR}))
          | trexp (A.OpExp{left,oper,right,pos}) = let
              val {ty=lt,...} = trexp left
              val {ty=rt,...} = trexp right
               in {exp=(), ty=checkOp oper (lt, rt, pos)}
              end
          | trexp (A.RecordExp{fields,typ,pos}) =
              (case lookupTy(tenv, typ, pos)
                 of ty as T.RECORD(tyfields, _) => let
                      fun fieldMatches((id:S.symbol,exp,_), (id',ty)) = let
                            val {ty=expTy,...} = trexp exp
                             in id = id' andalso tyMatches(expTy, actualTy ty)
                            end
                      in
                        if length fields <> length tyfields orelse
                            not(ListPair.all fieldMatches (fields, tyfields))
                        then
                          error pos "invalid record expression"
                        else ();
                        {exp=(), ty=ty}
                      end
                  | T.ERROR => {exp=(), ty=T.ERROR}
                  | _ => (error pos (S.name typ ^ " is not a record type");
                          {exp=(), ty=T.ERROR}))
          | trexp (A.ArrayExp{typ,size,init,pos}) =
              (case lookupTy(tenv, typ, pos)
                 of ty as T.ARRAY(elemTy, _) => let
                      val {ty=sizeTy,...} = trexp size
                      val {ty=initTy,...} = trexp init
                      in
                        if sizeTy <> T.INT then
                          error pos "array size is not of type int"
                        else ();
                        if not(tyMatches(initTy, actualTy elemTy)) then
                          error pos ("array init expression does not match " ^
                                     "element type")
                        else ();
                        {exp=(), ty=ty}
                      end
                  | T.ERROR => {exp=(), ty=T.ERROR}
                  | _ => (error pos (S.name typ ^ " is not an array type");
                          {exp=(), ty=T.ERROR}))
          | trexp (A.SeqExp exps) = let
              val trexps = map (trexp o #1) exps
              val ty = foldl (fn ({ty,...}, _) => ty) T.UNIT trexps
              (*val exp = Tr.seqExp(map #exp trexps)*)
               in {exp=(), ty=ty}
              end
          | trexp (A.AssignExp{var,exp,pos}) = let
              val {ty=lt,...} = trvar var
              val {ty=rt,...} = trexp exp
              in
                if not(tyMatches(rt, lt)) then
                  error pos "type mismatch"
                else ();
                {exp=(), ty=T.UNIT}
              end
          | trexp (A.IfExp{test,then',else'=NONE,pos}) = let
              val {ty=testTy,...} = trexp test
              val {ty=thenTy,...} = trexp then'
              in
                if testTy <> T.INT then
                  error pos "test expression in if is not of type int"
                else ();
                if thenTy <> T.UNIT then
                  error pos "then-expression must produce no value"
                else ();
                {exp=(), ty=T.UNIT}
              end
          | trexp (A.IfExp{test,then',else'=SOME(else'),pos}) = let
              val {ty=testTy,...} = trexp test
              val {ty=thenTy,...} = trexp then'
              val {ty=elseTy,...} = trexp else'
              val ty =
                (case (thenTy, elseTy)
                   of (T.NIL, T.RECORD _) => elseTy
                    | (T.RECORD _, T.NIL) => thenTy
                    | (T.ERROR, _)        => T.ERROR
                    | (_, T.ERROR)        => T.ERROR
                    | (a, b) =>
                        if a = b then a else
                          (error pos "types of if branches do not agree";
                           T.ERROR))
              in
                if testTy <> T.INT then
                  error pos "test expression in if is not of type int"
                else ();
                {exp=(), ty=ty}
              end
          | trexp (A.WhileExp{test,body,pos}) = let
              val {ty=testTy,...} = trexp test
              val {ty=bodyTy,...} = transExp(venv, tenv, level, {inLoop=true}) body
              in
                if testTy <> T.INT then
                  error pos "test expression in while is not of type int"
                else ();
                if bodyTy <> T.UNIT then
                  error pos "while body must produce no value"
                else ();
                {exp=(), ty=T.UNIT}
              end
          | trexp (A.ForExp{var,lo,hi,body,pos,...}) = let
              val access = Tr.allocLocal level true
              val venv' = S.enter(venv, var, E.VarEntry{access=access, ty=T.INT})
              val {ty=loTy,...} = trexp lo
              val {ty=hiTy,...} = trexp hi
              val {ty=bodyTy,...} = transExp(venv', tenv, level, {inLoop=true}) body
              in
                if loTy <> T.INT then
                  error pos "lower bound in for is not of type int"
                else ();
                if hiTy <> T.INT then
                  error pos "upper bound in for is not of type int"
                else ();
                if bodyTy <> T.UNIT then
                  error pos "for body must produce no value"
                else ();
                {exp=(), ty=T.UNIT}
              end
          | trexp (A.BreakExp pos) =
              (if not(#inLoop attrs) then
                 error pos "break outside loop"
               else ();
               {exp=(), ty=T.UNIT})
          | trexp (A.LetExp{decs,body,pos}) = let
              fun trdec (dec, {venv,tenv}) = transDec(venv, tenv, level, dec, attrs)
              val {venv=venv',tenv=tenv'} = foldl trdec {venv=venv,tenv=tenv} decs
               in transExp(venv', tenv', level, attrs) body
              end

        and trvar (A.SimpleVar(id,pos)) =
              (case S.look(venv, id)
                 of SOME(E.VarEntry{ty,...}) => {exp=(), ty=actualTy ty}
                  | _ => (error pos ("undefined variable " ^ S.name id);
                          {exp=(), ty=T.ERROR}))
          | trvar (A.FieldVar(var,id,pos)) =
              (case trvar var
                 of {ty=T.RECORD(fields,_),...} =>
                      (case List.find (fn (id',_) => id = id') fields
                         of SOME(_,ty) => {exp=(), ty=actualTy ty}
                          | NONE => (error pos ("no such field: " ^ S.name id);
                                     {exp=(), ty=T.ERROR}))
                  | {ty=T.ERROR,...} => {exp=(), ty=T.ERROR}
                  | _ => (error pos "attempt to select field of non-record value";
                          {exp=(), ty=T.ERROR}))
          | trvar (A.SubscriptVar(var,exp,pos)) =
              (case trvar var
                 of {ty=T.ARRAY(ty,_),...} => {exp=(), ty=actualTy ty}
                  | {ty=T.ERROR,...} => {exp=(), ty=T.ERROR}
                  | _ => (error pos "attempt to subscript non-array value";
                          {exp=(), ty=T.ERROR}))
         in trexp
        end

  and transDec (venv, tenv, level, A.VarDec{name,typ,init,pos,...}, attrs) = let
        val access = Tr.allocLocal level true
        val {ty=initTy,...} = transExp(venv, tenv, level, attrs) init
        val ty = 
          case (typ, initTy)
            of (NONE, T.NIL)  => (error pos "unconstrained nil"; T.ERROR)
             | (NONE, T.UNIT) => (error pos "cannot assign valueless expression";
                                  T.ERROR)
             | (NONE, _)      => initTy
             | (SOME(tyid,typos), _) => let
                 val ty = lookupTy(tenv, tyid, typos)
                 in
                   if not(tyMatches(initTy, ty)) then
                     error pos "expression does not match type constraint"
                   else ();
                   ty
                 end
         in {tenv=tenv,
             venv=S.enter(venv, name, E.VarEntry{access=access, ty=ty})}
        end
    | transDec (venv, tenv, level, A.TypeDec decs, _) = let
        fun enterHeader ({name,ty=_,pos}, tenv) =
              (case S.look(tenv, name)
                 of SOME(T.NAME(_, ref NONE)) =>
                      (error pos ("duplicate type definition: " ^ S.name name);
                       tenv)
                  | _ => S.enter(tenv, name, T.NAME(name, ref NONE)))
        val tenv' = foldl enterHeader tenv decs
        fun completeTy {name,ty,pos} = let
              fun illegalCycle (T.NAME(id, ref(SOME ty))) =
                    id = name orelse illegalCycle ty
                | illegalCycle _ = false
              val tyref =
                case S.look(tenv', name)
                  of SOME(T.NAME(_, ty)) => ty
                   | _ => impossible "expected name type"
              val ty = transTy(tenv', ty)
               in
                 tyref := SOME ty;
                 if illegalCycle ty then
                   error pos "illegal cycle in type declarations"
                 else ()
              end
         in map completeTy decs;
            {venv=venv, tenv=tenv'}
        end
    | transDec (venv, tenv, level, A.FunctionDec decs, _) = let
        fun trparam {name,typ,pos,escape=_} =
              {name=name, ty=lookupTy(tenv, typ, pos)}
        fun resultTy (SOME(tyid,pos)) = lookupTy(tenv, tyid, pos)
          | resultTy NONE = T.UNIT
        fun enterHeader ({name,params,result,body=_,pos}, (venv, entered)) =
              if List.exists (fn n => n = name) entered then
                (error pos ("duplicate function definition: " ^ S.name name);
                 (venv, entered))
              else let
                val formals = map (#ty o trparam) params
                val label = Temp.newlabel()
                val level' = Tr.newLevel {parent = level,
                                          name = label,
                                          formals = map (fn _ => true) formals}
                val entry = E.FunEntry{level = level',
                                       label = label,
                                       formals = formals,
                                       result = resultTy result}
                 in (S.enter(venv, name, entry), name::entered)
                end
        val (venv', _) = foldl enterHeader (venv, []) decs
        fun trbody ({name,params,result,body,pos}, venv) = let
              val params' = map trparam params
              val accesses =
                case S.look(venv, name)
                  of SOME(E.FunEntry{level,...}) => Tr.formals level
                   | _ => impossible "expected function"
              fun enterParam (({name,ty}, access), venv) =
                    S.enter(venv, name, E.VarEntry{access=access, ty=ty})
              val venv'' = foldl enterParam venv' (ListPair.zip(params', accesses))
              val {ty=bodyTy,...} = transExp(venv'', tenv, level, {inLoop=false}) body
              in
                if not(tyMatches(bodyTy, resultTy result)) then
                  error pos "function body type does not match result type"
                else ();
                venv'
              end
         in {venv=foldl trbody venv' decs, tenv=tenv}
        end

  and transTy (tenv, A.NameTy(id,pos)) = lookupNameTy(tenv, id, pos)
    | transTy (tenv, A.RecordTy fields) = let
        fun trfield {name,typ,pos,escape=_} = (name, lookupNameTy(tenv, typ, pos))
         in T.RECORD(map trfield fields, ref())
        end
    | transTy (tenv, A.ArrayTy(id,pos)) =
        T.ARRAY(lookupNameTy(tenv, id, pos), ref())

  local
    val level = Tr.newLevel {parent = Tr.outermost,
                             name = Temp.newlabel(),
                             formals = []}
    val attrs = {inLoop=false}
  in
    val transProg = ignore o transExp(E.baseVenv, E.baseTenv, level, attrs)
  end

end
