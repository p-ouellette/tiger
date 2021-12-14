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

  fun saytemp allocation t =
        case Temp.Table.find(allocation, t)
          of SOME r => r
           | NONE => Temp.makestring t

  fun alloc (instrs, frame) = let
        val fgraph = MakeGraph.instrs2graph instrs
        val (igraph, _) = Liveness.interferenceGraph fgraph
        (*
        val _ = print "\n[igraph]\n"
        val _ = Liveness.show(TextIO.stdOut, igraph, (saytemp Frame.tempMap))
        *)
        val (allocation, _) =
          Color.color {interference = igraph,
                       initial = Frame.tempMap,
                       spillCost = fn t => 1,
                       registers = Frame.registers}
         in (instrs, allocation)
        end
end
