signature LIVENESS =
sig
  datatype igraph = IGRAPH of {graph: Graph.graph,
                               tnode: Temp.temp -> Graph.node,
                               gtemp: Graph.graph -> Temp.temp,
                               moves: (Graph.graph * Graph.node) list}

  val liveness :
        (Graph.node list *
         Temp.Set.set Graph.Table.table *
         Temp.Set.set Graph.Table.table) ->
        (Temp.Set.set Graph.Table.table * Temp.Set.set Graph.Table.table)

  (*
  val interferenceGraph :
        Flow.flowgraph -> igraph * (Flow.Graph.node -> Temp.temp list)

  val show : outstream * igraph -> unit
  *)
end

structure Liveness : LIVENESS =
struct
  structure F = Flow
  structure G = Graph
  structure GT = G.Table
  structure TS = Temp.Set

  datatype igraph = IGRAPH of {graph: Graph.graph,
                               tnode: Temp.temp -> Graph.node,
                               gtemp: Graph.graph -> Temp.temp,
                               moves: (Graph.graph * Graph.node) list}

  fun liveness (nodes, defMap, useMap) = let
        fun initSets (n, (in_, out)) =
              (GT.enter(in_, n, TS.empty),
               GT.enter(out, n, TS.empty))
        fun doneIter (done, s, s') = done andalso TS.numItems s = TS.numItems s'
        fun liveIn (n, (inMap, outMap), done) = let
              val [in_, out, def, use] =
                map (fn m => GT.lookup(m, n)) [inMap, outMap, defMap, useMap]
              val in' = TS.union(use, TS.difference(out, def))
               in (GT.enter(inMap, n, in'), doneIter(done, in_, in'))
              end
        fun liveOut (n, (inMap, outMap), done) = let
              val out = GT.lookup(outMap, n)
              val succIn = map (fn n => GT.lookup(inMap, n)) (G.succ n)
              val out' = foldl TS.union TS.empty succIn
               in (GT.enter(outMap, n, out'), doneIter(done, out, out'))
              end
        fun computeSets (n, (maps, done)) = let
              val (inMap, done) = liveIn(n, maps, done)
              val (outMap, done) = liveOut(n, maps, done)
               in ((inMap, outMap), done)
              end
        fun iter maps = let
              val (maps', done) = foldl computeSets (maps, true) nodes
               in if done then maps' else iter maps'
              end
        val maps = foldl initSets (GT.empty, GT.empty) nodes
         in iter maps
        end

  (*
  fun interferenceGraph (F.FGRAPH{control,def=defMap,use=useMap,ismove}) = let
        val nodes = G.nodes control
        val (_, outMap) = liveness(nodes, defMap, useMap)

         in IGRAPH{graph=, tnode=, gtemp=, moves=}
        end
  *)
end
