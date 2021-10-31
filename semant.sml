structure Semant :
  sig val transProg : Absyn.exp -> Translate.Frame.frag list end =
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

  fun transExp (venv, tenv, level, break, comp) = let
        fun trexp (A.VarExp var)     = trvar var
          | trexp (A.NilExp)         = {exp=Tr.nilExp, ty=T.NIL}
          | trexp (A.IntExp i)       = {exp=Tr.intExp i, ty=T.INT}
          | trexp (A.StringExp(s,_)) = {exp=Tr.stringExp(comp, s), ty=T.STRING}

          | trexp (A.CallExp{func,args,pos}) =
              (case S.look(venv, func)
                 of SOME(E.FunEntry{level=flvl,label,formals,result}) => let
                      fun trarg(arg, ty, (exps,argn)) = let
                            val {exp, ty=argTy} = trexp arg
                            in
                              if not(tyMatches(argTy, ty)) then
                                error pos ("bad argument #" ^ Int.toString argn ^
                                           " to function " ^ S.name func ^
                                           " (type mismatch)")
                              else ();
                              (exp::exps, argn + 1)
                            end
                      val () =
                        if length args <> length formals then
                          error pos ("wrong number of arguments to function " ^
                                     S.name func)
                        else ();
                      val (args, _) = ListPair.foldl trarg ([], 1) (args, formals);
                      val exp = Tr.callExp(label, flvl, level, rev args)
                       in {exp=exp, ty=result}
                      end
                  | _ => (error pos ("undefined function " ^ S.name func);
                          {exp=Tr.nilExp, ty=T.ERROR}))

          | trexp (A.OpExp{left,oper,right,pos}) = let
              val {exp=lexp, ty=lt} = trexp left
              val {exp=rexp, ty=rt} = trexp right
              val ty = checkOp oper (lt, rt, pos)
              val exp = Tr.opExp(oper, ty) (lexp, rexp)
               in {exp=exp, ty=ty}
              end

          | trexp (A.RecordExp{fields,typ,pos}) =
              (case lookupTy(tenv, typ, pos)
                 of ty as T.RECORD(tyfields, _) => let
                      fun trfield((id,exp,pos), (id',ty), exps) = let
                            val {exp, ty=expTy} = trexp exp
                            in
                              if not(S.eq(id, id')) then
                                error pos ("expected record field " ^
                                           S.name id' ^ ", got " ^ S.name id)
                              else if not(tyMatches(expTy, actualTy ty)) then
                                error pos ("type of record field " ^ S.name id ^
                                           " does not match declared type")
                              else ();
                              exp::exps
                            end
                      val fields = ListPair.foldl trfield [] (fields, tyfields)
                      in
                        if length fields <> length tyfields then
                          error pos "wrong number of fields in record expression"
                        else ();
                        {exp=Tr.recordExp fields, ty=ty}
                      end
                  | T.ERROR => {exp=Tr.nilExp, ty=T.ERROR}
                  | _ => (error pos (S.name typ ^ " is not a record type");
                          {exp=Tr.nilExp, ty=T.ERROR}))

          | trexp (A.ArrayExp{typ,size,init,pos}) =
              (case lookupTy(tenv, typ, pos)
                 of ty as T.ARRAY(elemTy, _) => let
                      val {exp=sizeExp, ty=sizeTy} = trexp size
                      val {exp=initExp, ty=initTy} = trexp init
                      in
                        if sizeTy <> T.INT then
                          error pos "array size is not of type int"
                        else ();
                        if not(tyMatches(initTy, actualTy elemTy)) then
                          error pos ("array init expression does not match " ^
                                     "element type")
                        else ();
                        {exp=Tr.arrayExp(sizeExp, initExp), ty=ty}
                      end
                  | T.ERROR => {exp=Tr.nilExp, ty=T.ERROR}
                  | _ => (error pos (S.name typ ^ " is not an array type");
                          {exp=Tr.nilExp, ty=T.ERROR}))

          | trexp (A.SeqExp exps) = let
              val tr = map (trexp o #1) exps
              val exps = map #exp tr
              val ty = foldl (fn ({ty,...}, _) => ty) T.UNIT tr
               in {exp=Tr.seqExp exps, ty=ty}
              end

          | trexp (A.AssignExp{var,exp,pos}) = let
              val {exp=lexp, ty=lt} = trvar var
              val {exp=rexp, ty=rt} = trexp exp
              in
                if not(tyMatches(rt, lt)) then
                  error pos "type mismatch"
                else ();
                {exp=Tr.assignExp(lexp, rexp), ty=T.UNIT}
              end

          | trexp (A.IfExp{test,then',else'=NONE,pos}) = let
              val {exp=testExp, ty=testTy} = trexp test
              val {exp=thenExp, ty=thenTy} = trexp then'
              in
                if testTy <> T.INT then
                  error pos "test expression in if is not of type int"
                else ();
                if thenTy <> T.UNIT then
                  error pos "then-expression must produce no value"
                else ();
                {exp=Tr.ifThenExp(testExp, thenExp), ty=T.UNIT}
              end

          | trexp (A.IfExp{test,then',else'=SOME(else'),pos}) = let
              val {exp=testExp, ty=testTy} = trexp test
              val {exp=thenExp, ty=thenTy} = trexp then'
              val {exp=elseExp, ty=elseTy} = trexp else'
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
                {exp=Tr.ifExp(testExp, thenExp, elseExp), ty=ty}
              end

          | trexp (A.WhileExp{test,body,pos}) = let
              val {exp=testExp, ty=testTy} = trexp test
              val donelab = Temp.newlabel()
              val break = SOME donelab
              val {exp=bodyExp, ty=bodyTy} =
                transExp(venv, tenv, level, break, comp) body
              in
                if testTy <> T.INT then
                  error pos "test expression in while is not of type int"
                else ();
                if bodyTy <> T.UNIT then
                  error pos "while body must produce no value"
                else ();
                {exp=Tr.whileExp(testExp, bodyExp, donelab), ty=T.UNIT}
              end

          | trexp (A.ForExp{var,escape,lo,hi,body,pos}) = let
              val access = Tr.allocLocal level (!escape)
              val venv' = S.enter(venv, var, E.VarEntry{access=access, ty=T.INT})
              val {exp=loExp, ty=loTy} = trexp lo
              val {exp=hiExp, ty=hiTy} = trexp hi
              val donelab = Temp.newlabel()
              val break = SOME donelab
              val {exp=bodyExp, ty=bodyTy} =
                transExp(venv', tenv, level, break, comp) body
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
                {exp = Tr.forExp(access, loExp, hiExp, bodyExp, donelab),
                 ty = T.UNIT}
              end

          | trexp (A.BreakExp pos) = let
              val exp =
                case break
                  of SOME l => Tr.breakExp l
                   | NONE => (error pos "break outside loop"; Tr.nilExp)
               in {exp=exp, ty=T.UNIT}
              end

          | trexp (A.LetExp{decs,body,pos}) = let
              val {venv=venv', tenv=tenv', exps=assignExps} =
                transDecs(venv, tenv, level, break, comp) decs
              val {exp, ty} = transExp(venv', tenv', level, break, comp) body
               in {exp=Tr.seqExp(rev(exp::assignExps)), ty=ty}
              end

        and trvar (A.SimpleVar(id,pos)) =
              (case S.look(venv, id)
                 of SOME(E.VarEntry{access,ty}) =>
                      {exp=Tr.simpleVar(access, level), ty=actualTy ty}
                  | _ => (error pos ("undefined variable " ^ S.name id);
                          {exp=Tr.nilExp, ty=T.ERROR}))

          | trvar (A.FieldVar(var,id,pos)) =
              (case trvar var
                 of {ty=T.RECORD(fields,_), exp} => let
                      fun index (id, [], _) = NONE
                        | index (id, (id',ty)::fields, i) =
                            if S.eq(id, id') then SOME(i, ty)
                            else index(id, fields, i + 1)
                      in
                        case index(id, fields, 0)
                          of SOME(i, ty) =>
                              {exp=Tr.fieldVar(exp, i), ty=actualTy ty}
                           | NONE =>
                              (error pos ("no such field: " ^ S.name id);
                               {exp=Tr.nilExp, ty=T.ERROR})
                      end
                  | {ty=T.ERROR,...} => {exp=Tr.nilExp, ty=T.ERROR}
                  | _ => (error pos "attempt to select field of non-record value";
                          {exp=Tr.nilExp, ty=T.ERROR}))

          | trvar (A.SubscriptVar(var,exp,pos)) = let
              val {exp=varExp, ty=varTy} = trvar var
              val {exp=iExp, ty=iTy} = trexp exp
              val res =
                case varTy
                  of T.ARRAY(ty, _) =>
                       {exp=Tr.subscriptVar(varExp, iExp), ty=actualTy ty}
                   | T.ERROR => {exp=Tr.nilExp, ty=T.ERROR}
                   | _ => (error pos "attempt to subscript non-array value";
                           {exp=Tr.nilExp, ty=T.ERROR})
              in
                if iTy <> T.INT then
                  error pos "array subscript is not of type int"
                else ();
                res
              end

         in trexp
        end

  and transDec (level, break, comp)
               (A.VarDec{name,escape,typ,init,pos}, {venv,tenv,exps}) = let
        val access = Tr.allocLocal level (!escape)
        val {exp=initExp, ty=initTy} = transExp(venv, tenv, level, break, comp) init
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
        val varExp = Tr.simpleVar(access, level)
         in {tenv = tenv,
             venv = S.enter(venv, name, E.VarEntry{access=access, ty=ty}),
             exps = Tr.assignExp(varExp, initExp) :: exps}
        end

    | transDec (level, break, comp) (A.TypeDec decs, {venv,tenv,exps}) = let
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
                  (error pos "illegal cycle in type declarations";
                   (* prevent infinite loop in actualTy *)
                   tyref := SOME T.ERROR)
                else ()
              end
         in map completeTy decs;
            {venv=venv, tenv=tenv', exps=exps}
        end

    | transDec (level, break, comp) (A.FunctionDec decs, {venv,tenv,exps}) = let
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
        fun trbody ({name,params,result,body,pos}) = let
              val params' = map trparam params
              val (level, accesses) =
                case S.look(venv', name)
                  of SOME(E.FunEntry{level,...}) => (level, Tr.formals level)
                   | _ => impossible "expected function"
              fun enterParam ({name,ty}, access, venv) =
                    S.enter(venv, name, E.VarEntry{access=access, ty=ty})
              val venv'' = ListPair.foldl enterParam venv' (params', accesses)
              val {exp, ty=bodyTy} = transExp(venv'', tenv, level, NONE, comp) body
              in
                if not(tyMatches(bodyTy, resultTy result)) then
                  error pos "function body type does not match result type"
                else Tr.proc(comp, level, exp)
              end
         in app trbody decs;
            {venv=venv', tenv=tenv, exps=exps}
        end

  and transDecs (venv, tenv, level, break, comp) decs =
        foldl (transDec(level, break, comp)) {venv=venv, tenv=tenv, exps=[]} decs

  and transTy (tenv, A.NameTy(id,pos)) = lookupNameTy(tenv, id, pos)
    | transTy (tenv, A.RecordTy fields) = let
        fun trfield {name,typ,pos,escape=_} = (name, lookupNameTy(tenv, typ, pos))
         in T.RECORD(map trfield fields, ref())
        end
    | transTy (tenv, A.ArrayTy(id,pos)) =
        T.ARRAY(lookupNameTy(tenv, id, pos), ref())

  fun transProg prog = let
        val comp = Tr.newCompilation()
        val main = Tr.newLevel {parent = Tr.outermost,
                                 name = Temp.newlabel(),
                                 formals = []}
        val {exp,...} = transExp(E.baseVenv, E.baseTenv, main, NONE, comp) prog
         in Tr.proc(comp, main, exp);
            Tr.getResult comp
        end

end
