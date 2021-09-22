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

  structure S = Symbol
  structure T = Types

  type access = unit
  type ty = T.ty
  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty}

  val base_tenv =
    let val tenv = S.enter(S.empty,S.symbol("int"),T.INT)
                in S.enter(tenv,S.symbol("string"),T.STRING)
    end
  val base_venv =
    let val std_funcs =
          [("print",     ([T.STRING],             T.UNIT)),
           ("flush",     ([],                     T.UNIT)),
           ("getchar",   ([],                     T.STRING)),
           ("ord",       ([T.STRING],             T.INT)),
           ("chr",       ([T.INT],                T.STRING)),
           ("size",      ([T.STRING],             T.INT)),
           ("substring", ([T.STRING,T.INT,T.INT], T.STRING)),
           ("concat",    ([T.STRING,T.STRING],    T.STRING)),
           ("not",       ([T.INT],                T.INT)),
           ("exit",      ([T.INT],                T.UNIT))
          ]
        fun add_funentry ((id,(f,r)),venv) =
          S.enter(venv,S.symbol id,FunEntry{formals=f,result=r})

     in foldl add_funentry S.empty std_funcs
    end

end
