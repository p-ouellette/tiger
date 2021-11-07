structure MIPSFrame : FRAME =
struct

  structure A = Assem
  structure T = Tree

  val impossible = ErrorMsg.impossible
  val unimplemented = ErrorMsg.unimplemented

  (* frames *)

  datatype access = InFrame of int
                  | InReg of Temp.temp

  type frame = {name: Temp.label,
                formals: access list,
                nlocals: int ref}

  fun newFrame {name, formals} = let
        fun access(escape, (acc, offset)) =
              if escape then
                (InFrame offset :: acc, offset + 4)
              else
                (InReg(Temp.newtemp()) :: acc, offset)
        val (formals, _) = foldl access ([], 0) formals
        in {name = name,
            formals = rev formals,
            nlocals = ref 0}
        end

  fun name ({name,...}: frame) = name

  fun formals ({formals,...}: frame) = formals

  fun allocLocal ({nlocals,...}: frame) true =
        (nlocals := !nlocals + 1;
         InFrame(!nlocals * ~4))
    | allocLocal frame false = InReg(Temp.newtemp())

  (* registers *)

  type register = string

  local
    val registerNames =
      ["$zero", "$at", "$v0", "$v1", "$a0", "$a1", "$a2", "$a3",
       "$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7",
       "$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7",
       "$t8", "$t9", "$k0", "$k1", "$gp", "$sp", "$fp", "$ra"]

    fun mkReg (name, (map, regs)) = let
          val r = Temp.newtemp()
           in (Temp.Table.enter(map, r, name), r::regs)
          end

    val (tempMap,
        [zero, at, v0, v1, a0, a1, a2, a3,
         t0, t1, t2, t3, t4, t5, t6, t7,
         s0, s1, s2, s3, s4, s5, s6, s7,
         t8, t9, k0, k1, gp, sp, fp, ra]) =
      foldr mkReg (Temp.Table.empty, []) registerNames
  in
    val tempMap = tempMap
    val SP = sp
    val FP = fp
    val RV = v0
    val ZERO = zero
    val specialregs = [zero, v0, gp, sp, fp, ra]
    val argregs = [a0, a1, a2, a3]
    val calleesaves = [t0, t1, t2, t3, t4, t5, t6, t7, t8, t9]
    val callersaves = [s0, s1, s2, s3, s4, s5, s6, s7]
    val calldefs = RV::ra::callersaves
  end

  val wordSize = 4

  (* translation to IR trees *)

  datatype frag = PROC of {frame: frame, body: T.stm}
                | STRING of Temp.label * string

  fun expOfAccess (InFrame off) exp = T.MEM(T.BINOP(T.PLUS, exp, T.CONST off))
    | expOfAccess (InReg t) _ = T.TEMP t

  fun externalCall (name, args) = T.CALL(T.NAME(Temp.namedlabel name), args)

  fun argPos i =
        if i < 4 then T.TEMP(List.nth(argregs, i))
        else unimplemented()

  fun procEntryExit1 (frame, body) = body

  fun procEntryExit2 (frame, body) =
        body @ [A.OPER{assem="",
                       src=specialregs @ calleesaves,
                       dst=[], jump=NONE}]

  fun procEntryExit3 ({name,...}: frame, body) =
        {prolog = "PROCEDURE " ^ Symbol.name name ^ "\n.text\n",
         body = body,
         epilog = "END " ^ Symbol.name name ^ "\n"}

  fun string (lab, s) =
        concat [".data\n", Symbol.name lab, ":\n",
                ".word ", Int.toString(size s), "\n",
                ".ascii \"", String.toString s, "\"\n"]

end
