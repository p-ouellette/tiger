structure MakeGraph :
  sig
    val instrs2graph : Assem.instr list -> Flow.flowgraph
  end =
struct
  structure A = Assem
  structure F = Flow
  structure G = Graph
  structure GT = G.Table
  structure S = Symbol
  structure TS = Temp.Set

  val impossible = ErrorMsg.impossible

  fun instrs2graph instrs = let
        fun addInstr (instr,
                      (F.FGRAPH{control,def,use,ismove}, nodes, labmap)) = let
              val n = G.newNode control
              val _ =
                case nodes
                  of {node=prev, jumps=[]}::_ => G.mkEdge {from=prev, to=n}
                   | _ => ()
              val (dst, src) =
                case instr
                  of A.OPER{dst,src,...} => (TS.fromList dst, TS.fromList src)
                   | A.MOVE{dst,src,...} => (TS.singleton dst, TS.singleton src)
                   | A.LABEL _ => (TS.empty, TS.empty)
              val move =
                case instr
                  of A.MOVE _ => true
                   | _ => false
              val fgraph = F.FGRAPH{control = control,
                                    def = GT.enter(def, n, dst),
                                    use = GT.enter(use, n, src),
                                    ismove = GT.enter(ismove, n, move)}
              val jumps =
                case instr
                  of A.OPER{jump=SOME j,...} => j
                   | _ => []
              val nodes = {node=n, jumps=jumps} :: nodes
              val labmap =
                case instr
                  of A.LABEL{lab,...} => S.enter(labmap, lab, n)
                   | _ => labmap
               in (fgraph, nodes, labmap)
              end

        val fgraph = F.FGRAPH{control=G.newGraph(),
                              def=GT.empty, use=GT.empty, ismove=GT.empty}
        val (fgraph, nodes, labmap) = foldl addInstr (fgraph, [], S.empty) instrs

        fun mkJumps {node=from, jumps=labs} =
              app (fn l => G.mkEdge {from=from, to=S.look(labmap, l)}) labs

         in app mkJumps nodes;
            fgraph
        end
end
