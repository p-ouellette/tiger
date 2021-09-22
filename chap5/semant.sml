structure Semant :
  sig val transProg : Absyn.exp -> Types.ty end =
struct

  structure A = Absyn
  structure E = Env
  structure S = Symbol

  structure Translate = struct type exp = unit end

  type expty = {exp: Translate.exp, ty: Types.ty}

  val error = ErrorMsg.error
  val impossible = ErrorMsg.impossible

  fun argMatches (Types.NIL,Types.RECORD _) = true
    | argMatches (_,Types.NIL) = impossible "formal parameter of type nil"
    | argMatches (a,b) = a = b

  local
    fun checkArithOp (Types.INT,Types.INT,_) = Types.INT
      | checkArithOp (_,_,pos) = (error pos "integer required"; Types.INT)

    fun checkEqOp (Types.NIL,Types.RECORD a,_) = Types.RECORD a
      | checkEqOp (Types.RECORD a,Types.NIL,_) = Types.RECORD a
      | checkEqOp (Types.NIL,Types.NIL,pos) =
          (error pos "cannot determine type of nil"; Types.INT)
      | checkEqOp (a,b,pos) =
          if a = b then a else (error pos "type mismatch"; Types.INT)

    fun checkOrdOp (Types.INT,Types.INT,_) = Types.INT
      | checkOrdOp (Types.STRING,Types.STRING,_) = Types.STRING
      | checkOrdOp (_,_,pos) = (error pos "invalid comparison"; Types.INT)
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

  and transExp(venv,tenv) =
    let fun trexp (A.VarExp var) = trvar var
          | trexp (A.NilExp) = {exp=(), ty=Types.NIL}
          | trexp (A.IntExp _) = {exp=(), ty=Types.INT}
          | trexp (A.StringExp _) = {exp=(), ty=Types.STRING}
          | trexp (A.CallExp{func,args,pos}) =
              (case S.look(venv,func)
                of SOME(E.FunEntry{formals,result}) =>
                    let fun checkArg(arg,ty,argn) =
                          (if not(argMatches(#ty(trexp arg), ty)) then
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
                         {exp=(), ty=Types.INT}))
          | trexp (A.OpExp{left,oper,right,pos}) =
              let val {ty=tyLeft,...} = trexp left
                  val {ty=tyRight,...} = trexp right
              in
                {exp=(), ty=checkOp oper (tyLeft,tyRight,pos)}
              end
          (*| trexp (A.RecordExp{fields,typ}) =*)
          | trexp (A.SeqExp exps) =
              let val ty = foldl (fn ((e,_),_) => #ty(trexp e)) Types.UNIT exps
              in {exp=(), ty=ty}
              end

        and trvar (A.SimpleVar(id,pos)) =
              (case S.look(venv,id)
                of SOME(E.VarEntry{ty}) => {exp=(), ty=actual_ty ty}
                 | _ => (error pos ("undefined variable " ^ S.name id);
                         {exp=(), ty=Types.INT}))
          | trvar (A.FieldVar(v,id,pos)) =
              (case trvar v
                of {ty=Types.RECORD(fields,_),...} =>
                    (case List.find (fn (id',_) => id = id') fields
                      of SOME(_,ty) => {exp=(), ty=ty}
                       | NONE => (error pos ("no such field: " ^ S.name id);
                                  {exp=(), ty=Types.INT}))
                 | _ => (error pos "attempt to select field of non-record value";
                         {exp=(), ty=Types.INT}))
          | trvar (A.SubscriptVar(v,exp,pos)) =
              (case trvar v
                of {ty=Types.ARRAY(ty,_),...} => {exp=(), ty=ty}
                 | _ => (error pos "attempt to subscript non-array value";
                         {exp=(), ty=Types.INT}))
     in trexp
    end

end
