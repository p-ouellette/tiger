structure Semant :
  sig val transProg : Absyn.exp -> unit end =
struct

  structure A = Absyn
  structure E = Env
  structure S = Symbol

  type venv = E.enventry S.table
  type tenv = Types.ty S.table

  structure Translate = struct type exp = unit end

  type expty = {exp: Translate.exp, ty: Types.ty}

  val error = ErrorMsg.error

  fun checkInt({exp,ty},pos) =
    case ty of Types.INT => ()
             | _ => error pos "integer required"

  fun actual_ty ty = ty

  fun transProg exp = (transExp(E.base_venv,E.base_tenv) exp; ())

  and transExp(venv,tenv) =
    let fun trexp (A.NilExp) = {exp=(),ty=Types.NIL}
          | trexp (A.IntExp _) = {exp=(),ty=Types.INT}
          | trexp (A.StringExp _) = {exp=(),ty=Types.STRING}
          | trexp (A.OpExp{left,oper=A.PlusOp,right,pos}) =
              (checkInt(trexp left, pos);
               checkInt(trexp right, pos);
               {exp=(),ty=Types.INT})
          (*| trexp (A.RecordExp{fields,typ}) =*)
        and trvar (A.SimpleVar(id,pos)) =
              (case S.look(venv,id)
                of SOME(E.VarEntry{ty}) =>
                    {exp=(),ty=actual_ty ty}
                 | NONE => (error pos ("undefined variable " ^ S.name id);
                    {exp=(),ty=Types.INT}))
     in trexp
    end

end
