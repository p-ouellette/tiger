signature REG_ALLOC =
sig
  structure Frame : FRAME
  type allocation = Frame.register Temp.Table.table

  (* Returns the instruction list updated to fetch and store spills and a
   * register allocation.
   *)
  val alloc : Assem.instr list * Frame.frame -> Assem.instr list * allocation
end

structure RegAlloc : REG_ALLOC =
struct
  structure Frame = MIPSFrame
  type allocation = Frame.register Temp.Table.table

  fun alloc (instrs, frame) =
        raise Fail "unimplemented"
end
