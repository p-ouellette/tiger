structure MIPSFrame : FRAME =
struct

  datatype access = InFrame of int
                  | InReg of Temp.temp

  type frame = {name: Temp.label,
                formals: access list,
                nLocals: int ref}

  fun newFrame {name, formals} = let
        fun access(escape, (acc, offset)) =
              (if escape then
                 ((InFrame offset)::acc, offset + 4)
               else
                 ((InReg(Temp.newtemp())::acc), offset))
        val (formals, _) = foldl access ([], 0) formals
        in {name = name,
            formals = formals,
            nLocals = ref 0}
        end

  fun name ({name,...}: frame) = name

  fun formals ({formals,...}: frame) = formals

  fun allocLocal ({nLocals,...}: frame) true =
        (nLocals := !nLocals + 1;
         InFrame(!nLocals * ~4))
    | allocLocal frame false = InReg(Temp.newtemp())

end