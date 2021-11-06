signature GRAPH =
sig
  type graph
  type node

  val nodes: graph -> node list
  val succ: node -> node list
  val pred: node -> node list
  val adj: node -> node list   (* succ+pred *)
  val eq: node*node -> bool

  val newGraph: unit -> graph
  val newNode : graph -> node
  exception GraphEdge
  val mkEdge: {from: node, to: node} -> unit
  val rmEdge: {from: node, to: node} -> unit

  structure Table : TABLE
  sharing type Table.key = node

  val nodename : node -> string  (* for debugging only *)

end
