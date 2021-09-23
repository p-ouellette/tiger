structure Semant :
  sig val transProg : Absyn.exp -> Types.ty end =
struct

  structure A = Absyn
  structure E = Env
  structure S = Symbol
  structure T = Types

  structure Translate = struct type exp = unit end

  type expty = {exp: Translate.exp, ty: T.ty}

  val error = ErrorMsg.error
  val impossible = ErrorMsg.impossible

  fun tyMatches (T.NIL, T.RECORD _) = true
    | tyMatches (_, T.NIL)          = impossible "nil declared type"
    | tyMatches (ty, declared) = ty = declared

  local
    fun checkArith (T.INT, T.INT,_) = T.INT
      | checkArith (_, _, pos) = (error pos "integer required"; T.INT)

    fun checkEq (T.NIL, T.RECORD a, _) = T.RECORD a
      | checkEq (T.RECORD a, T.NIL, _) = T.RECORD a
      | checkEq (T.NIL, T.NIL, pos) =
          (error pos "cannot determine type of nil"; T.INT)
      | checkEq (a, b, pos) =
          if a = b then a else (error pos "type mismatch"; T.INT)

    fun checkOrd (T.INT, T.INT, _)       = T.INT
      | checkOrd (T.STRING, T.STRING, _) = T.STRING
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

  fun actualTy ty = ty

  fun lookupTy (tenv, id, pos) =
        (case S.look(tenv, id)
           of SOME ty => ty
            | NONE => (error pos ("undefined type " ^ S.name id); T.INT))

  fun transProg exp = #ty(transExp(E.base_venv, E.base_tenv) exp)

  and transExp (venv, tenv) = let
        fun trexp (A.VarExp var)  = trvar var
          | trexp (A.NilExp)      = {exp=(), ty=T.NIL}
          | trexp (A.IntExp _)    = {exp=(), ty=T.INT}
          | trexp (A.StringExp _) = {exp=(), ty=T.STRING}
          | trexp (A.CallExp{func,args,pos}) =
              (case S.look(venv, func)
                 of SOME(E.FunEntry{formals,result}) => let
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
                          {exp=(), ty=T.INT}))
          | trexp (A.OpExp{left,oper,right,pos}) = let
              val {ty=tyl,...} = trexp left
              val {ty=tyr,...} = trexp right
               in {exp=(), ty=checkOp oper (tyl, tyr, pos)}
              end
          (*| trexp (A.RecordExp{fields,typ}) =*)
          | trexp (A.SeqExp exps) = let
              val ty = foldl (fn ((e,_),_) => #ty(trexp e)) T.UNIT exps
               in {exp=(), ty=ty}
              end
          | trexp (A.LetExp{decs,body,pos}) = let
              fun trdec (dec, {venv,tenv}) = transDec(venv, tenv, dec)
              val {venv=venv',tenv=tenv'} = foldl trdec {venv=venv,tenv=tenv} decs
               in transExp(venv', tenv') body
              end

        and trvar (A.SimpleVar(id,pos)) =
              (case S.look(venv, id)
                 of SOME(E.VarEntry{ty}) => {exp=(), ty=actualTy ty}
                  | _ => (error pos ("undefined variable " ^ S.name id);
                          {exp=(), ty=T.INT}))
          | trvar (A.FieldVar(v,id,pos)) =
              (case trvar v
                 of {ty=T.RECORD(fields,_),...} =>
                      (case List.find (fn (id',_) => id = id') fields
                         of SOME(_,ty) => {exp=(), ty=ty}
                          | NONE => (error pos ("no such field: " ^ S.name id);
                                     {exp=(), ty=T.INT}))
                  | _ => (error pos "attempt to select field of non-record value";
                          {exp=(), ty=T.INT}))
          | trvar (A.SubscriptVar(v,exp,pos)) =
              (case trvar v
                 of {ty=T.ARRAY(ty,_),...} => {exp=(), ty=ty}
                  | _ => (error pos "attempt to subscript non-array value";
                          {exp=(), ty=T.INT}))
         in trexp
        end

  and transDec (venv, tenv, A.VarDec{name,typ=NONE,init,pos,...}) = let
        val {ty,...} = transExp(venv, tenv) init
        in
          case ty
            of T.NIL  => (error pos "unconstrained nil";
                          {tenv=tenv, venv=venv})
             | T.UNIT => (error pos "cannot assign valueless expression";
                          {tenv=tenv, venv=venv})
             | _      => {tenv=tenv,
                          venv=S.enter(venv, name, E.VarEntry{ty=ty})}
        end
    | transDec (venv, tenv, A.VarDec{name,typ=SOME(tyid,typos),init,pos,...}) =
        let
          val {ty,...} = transExp(venv, tenv) init
          val ty' = lookupTy(tenv, tyid, typos)
        in
          if not(tyMatches(ty, ty')) then
            error pos "expression does not match type constraint"
          else ();
          {tenv=tenv,
           venv=S.enter(venv, name, E.VarEntry{ty=ty'})}
        end
    | transDec (venv, tenv, A.TypeDec decs) = let
        fun trdec ({name,ty,pos=_}, {venv,tenv}) =
              {venv=venv,
               tenv=S.enter(tenv, name, transTy(tenv,ty))}
         in foldl trdec {venv=venv, tenv=tenv} decs
        end

  and transTy (tenv, A.NameTy(id,pos)) = lookupTy(tenv, id, pos)
    | transTy (tenv, A.RecordTy fields) = let
        fun trfield {name,typ,pos,escape=_} = (name, lookupTy(tenv, typ, pos))
         in T.RECORD(map trfield fields, ref())
        end
    | transTy (tenv, A.ArrayTy(id,pos)) =
        T.ARRAY(lookupTy(tenv, id, pos), ref())

end
