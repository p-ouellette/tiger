signature ENV =
sig
  type ty
  datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                    | FunEntry of {level: Translate.level,
                                   label: Temp.label,
                                   formals: ty list, result: ty}

  val baseTenv : ty Symbol.table
  val baseVenv : enventry Symbol.table
end

structure Env : ENV =
struct

  structure S = Symbol
  structure T = Types

  type ty = T.ty
  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty}

  val baseTenv = let
    val tenv = S.enter(S.empty, S.symbol("int"), T.INT)
     in        S.enter(tenv, S.symbol("string"), T.STRING)
    end
  val baseVenv = let
    val funcs = [("print",     ([T.STRING],             T.UNIT)),
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
    fun enterFunc ((id,(ft,rt)), venv) =
          S.enter(venv, S.symbol id, FunEntry{formals=ft,result=rt})
    in
      foldl enterFunc S.empty funcs
    end

end
