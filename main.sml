structure Main :
  sig val compile : string -> unit end =
struct

  structure Codegen = MIPSGen
  structure Frame = MIPSFrame

  fun saytemp t =
        case Temp.Table.find(Frame.tempMap, t)
          of SOME r => r
           | NONE => Temp.makestring t

  fun emitFrag out (Frame.PROC{body,frame}) = let
        val _ = print("\n[emit " ^ Symbol.name(Frame.name frame) ^ "]\n")
        val _ = print "[tree]\n"
        val _ = PrintTree.printTree(out, body)
        val _ = print "\n[canon]\n"
        val stms = Canon.linearize body
        val stms = Canon.traceSchedule(Canon.basicBlocks stms)
        val _ = app (fn s => PrintTree.printTree(out, s)) stms
        val _ = print "\n[assem]\n"
        val instrs = List.concat(map (Codegen.codegen frame) stms)
        val instrs = Frame.procEntryExit2(frame, instrs)
        val Flow.FGRAPH{control,def=defMap,use=useMap,...} = MakeGraph.instrs2graph instrs
        val nodes = Graph.nodes control
        val (inMap, outMap) = Liveness.liveness(nodes, defMap, useMap)
        fun printNode n = let
              fun listStr items = "[" ^ String.concatWith "," items ^ "]"
              val succ = listStr (map Graph.nodename (Graph.succ n))
              val pred = listStr (map Graph.nodename (Graph.pred n))
              val liveIn = Graph.Table.lookup(inMap, n)
              val liveOut = Graph.Table.lookup(outMap, n)
              val liveIn = listStr (map saytemp (Temp.Set.toList liveIn))
              val liveOut = listStr (map saytemp (Temp.Set.toList liveOut))
              val node = concat [Graph.nodename n,
                                 ": succ=", succ, "\tpred=", pred,
                                 "\tin=", liveIn, "\tout=", liveOut, "\n"]
               in TextIO.output(out, node)
              end
        val _ = app printNode nodes
        val {prolog, body, epilog} = Frame.procEntryExit3(frame, instrs)
        val format = Assem.format saytemp
         in TextIO.output(out, prolog);
            app (fn i => TextIO.output(out, format i)) body;
            TextIO.output(out, epilog)
        end
    | emitFrag out (Frame.STRING(lab, s)) =
        TextIO.output(out, Frame.string(lab, s))

  fun compile filename = let
        val absyn = Parse.parse filename
        val frags = Semant.transProg absyn
        in
          if !ErrorMsg.anyErrors then ()
          else app (emitFrag TextIO.stdOut) frags
        end

end
