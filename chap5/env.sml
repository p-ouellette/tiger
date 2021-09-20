signature ENV =
sig
  type access
  type ty
  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty}

  val base_tenv : ty Symbol.table
  val base_venv : enventry Symbol.table
end

structure Env : ENV =
struct

  type access = unit
  type ty = Types.ty
  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty}

  structure S = Symbol

  val base_tenv = let val tenv = S.enter(S.empty,S.symbol("int"),Types.INT)
                   in S.enter(tenv,S.symbol("string"),Types.STRING)
                  end
  val base_venv = S.empty

end
