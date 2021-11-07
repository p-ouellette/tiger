structure MakeGraph :
  sig
    val instrs2graph : Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
  end =
struct
  structure A = Assem
  structure F = Flow
  structure G = F.Graph
  structure S = Symbol
  structure T = G.Table

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
                  of A.OPER{dst,src,...} => (dst, src)
                   | A.MOVE{dst,src,...} => ([dst], [src])
                   | A.LABEL _ => ([], [])
              val move =
                case instr
                  of A.MOVE _ => true
                   | _ => false
              val fgraph = F.FGRAPH{control = control,
                                    def = T.enter(def, n, dst),
                                    use = T.enter(use, n, src),
                                    ismove = T.enter(ismove, n, move)}
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
                              def=T.empty, use=T.empty, ismove=T.empty}
        val (fgraph, nodes, labmap) = foldl addInstr (fgraph, [], S.empty) instrs

        fun mkJumps {node=from, jumps=labs} = let
              fun mkJump lab = let
                    val to =
                      case S.look(labmap, lab)
                        of SOME n => n
                         | NONE => impossible "jump to nonexistent label"
                     in G.mkEdge {from=from, to=to}
                    end
               in app mkJump labs
              end

         in app mkJumps nodes;
            (fgraph, map #node nodes)
        end
end
