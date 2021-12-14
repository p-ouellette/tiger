structure Main =
struct

  structure Codegen = MIPSGen
  structure Frame = MIPSFrame

  fun saytemp allocation t =
        case Temp.Table.find(allocation, t)
          of SOME r => r
           | NONE => Temp.makestring t

  fun emitFrag out (Frame.PROC{body,frame}) = let
        val stms = Canon.linearize body
        val stms = Canon.traceSchedule(Canon.basicBlocks stms)
        val instrs = List.concat(map (Codegen.codegen frame) stms)
        val instrs = Frame.procEntryExit2(frame, instrs)
        val (instrs, allocation) = RegAlloc.alloc(instrs, frame)
        val {prolog, body, epilog} = Frame.procEntryExit3(frame, instrs)
        val format = Assem.format (saytemp allocation)
        val _ = TextIO.output(out, "\n")
         in TextIO.output(out, prolog);
            app (fn i => TextIO.output(out, format i)) body;
            TextIO.output(out, epilog)
        end
    | emitFrag out (Frame.STRING(lab, s)) =
        TextIO.output(out, Frame.string(lab, s))

  fun compile filename = let
        val absyn = Parse.parse filename
        val frags = Semant.transProg absyn
        val (procs, strs) =
          List.partition (fn Frame.PROC _ => true | _ => false) frags
        in
          if !ErrorMsg.anyErrors then () else let
            val out = TextIO.stdOut
            in
              TextIO.output(out, ".data\n");
              app (emitFrag out) procs;
              TextIO.output(out, "\n.text\n");
              app (emitFrag out) strs
            end
        end

  fun main (cmd, args) = (app compile args; 0)

end
