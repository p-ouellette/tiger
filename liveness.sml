signature LIVENESS =
sig
  datatype igraph = IGRAPH of {graph: Graph.graph,
                               tnode: Temp.temp -> Graph.node,
                               gtemp: Graph.node -> Temp.temp,
                               moves: (Graph.node * Graph.node) list}

  val interferenceGraph :
        Flow.flowgraph -> igraph * (Graph.node -> Temp.Set.set)

  val show : TextIO.outstream * igraph * (Temp.temp -> string) -> unit
end

structure Liveness : LIVENESS =
struct
  structure F = Flow
  structure G = Graph
  structure GT = Graph.Table
  structure TS = Temp.Set
  structure TT = Temp.Table

  datatype igraph = IGRAPH of {graph: Graph.graph,
                               tnode: Temp.temp -> Graph.node,
                               gtemp: Graph.node -> Temp.temp,
                               moves: (Graph.node * Graph.node) list}

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
        val (_, outMap) = iter maps
         in outMap
        end

  fun interferenceGraph (F.FGRAPH{control,def=defMap,use=useMap,ismove}) = let
        val graph = G.newGraph()
        fun addNode (t, (tnode, gtemp)) = let
              val n = G.newNode graph
               in (TT.enter(tnode, t, n),
                   GT.enter(gtemp, n, t))
              end
        val temps = GT.foldl TS.union (GT.foldl TS.union TS.empty defMap) useMap
        val (tnode, gtemp) = TS.foldl addNode (TT.empty, GT.empty) temps
        val tnode = fn t => TT.lookup(tnode, t)
        val gtemp = fn n => GT.lookup(gtemp, n)
        val fnodes = G.nodes control
        val outMap = liveness(fnodes, defMap, useMap)
        fun mkEdges fnode = let
              val [liveOut, def, use] =
                map (fn m => GT.lookup(m, fnode)) [outMap, defMap, useMap]
              val move = GT.lookup(ismove, fnode)
              fun mkEdge a b =
                    if a = b orelse move andalso b = TS.minItem use then ()
                    else
                      G.mkEdge {from=tnode a, to=tnode b}
               in TS.app (fn d => TS.app (mkEdge d) liveOut) def
              end
        fun getMove fnode = let
              val [dst, src] =
                map (fn m => tnode(TS.minItem(GT.lookup(m, fnode))))
                    [defMap, useMap]
               in (dst, src)
              end
        val moveNodes = List.filter (fn n => GT.lookup(ismove, n)) fnodes
        val moves = map getMove moveNodes

         in app mkEdges fnodes;
            (IGRAPH{graph=graph, tnode=tnode, gtemp=gtemp, moves=moves},
             fn n => GT.lookup(outMap, n))
        end

  fun show (out, IGRAPH{graph,gtemp,...}, saytemp) = let
        fun printNode n = let
              val adj = TS.toList(TS.fromList(map gtemp (G.adj n)))
              val adj = String.concatWith "," (map saytemp adj)
              val line = concat [saytemp (gtemp n), ": [", adj, "]\n"]
               in TextIO.output(out, line)
              end
         in app printNode (G.nodes graph)
        end
end
