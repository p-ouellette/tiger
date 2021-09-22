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

  fun tyMatches (T.NIL,T.RECORD _) = true
    | tyMatches (_,T.NIL) = impossible "nil declared type"
    | tyMatches (a,b) = a = b

  local
    fun checkArithOp (T.INT,T.INT,_) = T.INT
      | checkArithOp (_,_,pos) = (error pos "integer required"; T.INT)

    fun checkEqOp (T.NIL,T.RECORD a,_) = T.RECORD a
      | checkEqOp (T.RECORD a,T.NIL,_) = T.RECORD a
      | checkEqOp (T.NIL,T.NIL,pos) =
          (error pos "cannot determine type of nil"; T.INT)
      | checkEqOp (a,b,pos) =
          if a = b then a else (error pos "type mismatch"; T.INT)

    fun checkOrdOp (T.INT,T.INT,_) = T.INT
      | checkOrdOp (T.STRING,T.STRING,_) = T.STRING
      | checkOrdOp (_,_,pos) = (error pos "invalid comparison"; T.INT)
  in
    fun checkOp A.PlusOp = checkArithOp
      | checkOp A.MinusOp = checkArithOp
      | checkOp A.TimesOp = checkArithOp
      | checkOp A.DivideOp = checkArithOp
      | checkOp A.EqOp = checkEqOp
      | checkOp A.NeqOp = checkEqOp
      | checkOp A.LtOp = checkOrdOp
      | checkOp A.LeOp = checkOrdOp
      | checkOp A.GtOp = checkOrdOp
      | checkOp A.GeOp = checkOrdOp
  end

  fun actual_ty ty = ty

  fun transProg exp = #ty(transExp(E.base_venv,E.base_tenv) exp)

  and transExp (venv,tenv) =
    let fun trexp (A.VarExp var) = trvar var
          | trexp (A.NilExp) = {exp=(), ty=T.NIL}
          | trexp (A.IntExp _) = {exp=(), ty=T.INT}
          | trexp (A.StringExp _) = {exp=(), ty=T.STRING}
          | trexp (A.CallExp{func,args,pos}) =
              (case S.look(venv,func)
                of SOME(E.FunEntry{formals,result}) =>
                    let fun checkArg(arg,ty,argn) =
                          (if not(tyMatches(#ty(trexp arg), ty)) then
                             error pos ("bad argument #" ^ Int.toString argn ^
                                        " to function " ^ S.name func ^
                                        " (type mismatch)")
                           else ();
                           argn + 1)
                    in if length args <> length formals then
                         error pos ("wrong number of arguments to function " ^
                                    S.name func)
                       else ();
                       ListPair.foldl checkArg 1 (args,formals);
                       {exp=(), ty=result}
                    end
                 | _ => (error pos ("undefined function " ^ S.name func);
                         {exp=(), ty=T.INT}))
          | trexp (A.OpExp{left,oper,right,pos}) =
              let val {ty=tyLeft,...} = trexp left
                  val {ty=tyRight,...} = trexp right
              in
                {exp=(), ty=checkOp oper (tyLeft,tyRight,pos)}
              end
          (*| trexp (A.RecordExp{fields,typ}) =*)
          | trexp (A.SeqExp exps) =
              let val ty = foldl (fn ((e,_),_) => #ty(trexp e)) T.UNIT exps
               in {exp=(), ty=ty}
              end
          | trexp (A.LetExp{decs,body,pos}) =
              let val {venv=venv',tenv=tenv'} =
                    foldl (fn (dec,{venv,tenv}) => transDec(venv,tenv,dec))
                      {venv=venv,tenv=tenv} decs
               in transExp(venv',tenv') body
              end

        and trvar (A.SimpleVar(id,pos)) =
              (case S.look(venv,id)
                of SOME(E.VarEntry{ty}) => {exp=(), ty=actual_ty ty}
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

  and transDec (venv,tenv,A.VarDec{name,typ=NONE,init,pos,...}) =
        (case transExp(venv,tenv) init
          of {ty=T.NIL,...} => (error pos "unconstrained nil";
                                {tenv=tenv, venv=venv})
           | {ty=T.UNIT,...} => (error pos "cannot assign valueless expression";
                                 {tenv=tenv, venv=venv})
           | {ty,...} => {tenv=tenv,
                          venv=S.enter(venv,name,E.VarEntry{ty=ty})})
    | transDec (venv,tenv,A.VarDec{name,typ=SOME(tyid,typos),init,pos,...}) =
        (case (transExp(venv,tenv) init, S.look(tenv,tyid))
          of ({ty,...},SOME ty') =>
              (if not(tyMatches(ty, ty')) then
                 error pos "expression does not match type constraint"
               else ();
               {tenv=tenv,
                venv=S.enter(venv,name,E.VarEntry{ty=ty'})})
           | (_,NONE) =>
              (error typos ("undefined type " ^ S.name tyid);
               {tenv=tenv, venv=venv}))

end
