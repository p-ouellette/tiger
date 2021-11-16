signature COLOR =
sig
  structure Frame : FRAME

  type allocation = Frame.register Temp.Table.table

  (* Returns a register allocation and a list of spills. *)
  val color : {interference: Liveness.igraph,
               initial: allocation,
               spillCost: Graph.node -> int,
               registers: Frame.register list} -> allocation * Temp.temp list
end

structure Color : COLOR =
struct
  structure Frame = MIPSFrame
  structure L = Liveness

  type allocation = Frame.register Temp.Table.table

  fun color {interference=L.IGRAPH{graph,tnode,gtemp,moves},
             initial, spillCost, registers} =
        raise Fail "unimplemented"
end
