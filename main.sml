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
        val stms = Canon.linearize body
        val stms = Canon.traceSchedule(Canon.basicBlocks stms)
        val _ = print "\n[canon]\n"
        val _ = app (fn s => PrintTree.printTree(out, s)) stms
        val instrs = List.concat(map (Codegen.codegen frame) stms)
        val instrs = Frame.procEntryExit2(frame, instrs)
        val fgraph = MakeGraph.instrs2graph instrs
        val (igraph, outMap) = Liveness.interferenceGraph fgraph
        val _ = print "\n[igraph]\n"
        val _ = Liveness.show(TextIO.stdOut, igraph, saytemp)
        val {prolog, body, epilog} = Frame.procEntryExit3(frame, instrs)
        val format = Assem.format saytemp
        val _ = print "\n[assem]\n"
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
