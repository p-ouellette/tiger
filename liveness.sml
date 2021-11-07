signature LIVENESS =
sig
  datatype igraph = IGRAPH of {graph: Graph.graph,
                               tnode: Temp.temp -> Graph.node,
                               gtemp: Graph.graph -> Temp.temp,
                               moves: (Graph.graph * Graph.node) list}

  val interferenceGraph :
        Flow.flowgraph -> igraph * (Flow.Graph.node -> Temp.temp list)

  val show : outstream * igraph -> unit
end

structure Liveness : LIVENESS =
struct

  datatype igraph = IGRAPH of {graph: Graph.graph,
                               tnode: Temp.temp -> Graph.node,
                               gtemp: Graph.graph -> Temp.temp,
                               moves: (Graph.graph * Graph.node) list}

end
