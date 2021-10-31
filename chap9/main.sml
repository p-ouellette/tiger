structure Main :
  sig
    val printAbsyn : string -> unit
    val compile : string -> unit
  end =
struct

  structure Codegen = MIPSGen
  structure F = Translate.Frame

  structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
  structure Lex = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
  structure TigerP = Join(structure ParserData = TigerLrVals.ParserData
                          structure Lex = Lex
                          structure LrParser = LrParser)

  fun parse filename = let
        val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
        val file = TextIO.openIn filename
        fun get _ = TextIO.input file
        fun parseError(s, p1, p2) = ErrorMsg.error p1 s
        val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
        val (absyn, _) = TigerP.parse(30, lexer, parseError, ())
         in TextIO.closeIn file;
            absyn
        end handle LrParser.ParseError => raise ErrorMsg.Error

  fun printAbsyn filename = PrintAbsyn.print(TextIO.stdOut, parse filename)

  fun saytemp t =
        case Temp.Table.look(F.tempMap, t)
          of SOME r => r
           | NONE => Temp.makestring t

  fun emitFrag out (F.PROC{body,frame}) = let
        val _ = print("\n[emit " ^ Symbol.name(F.name frame) ^ "]\n")
        val _ = print "[tree]\n"
        val _ = PrintTree.printTree(out, body)
        val _ = print "\n[canon]\n"
        val stms = Canon.linearize body
        val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
        val _ = app (fn s => PrintTree.printTree(out, s)) stms'
        val _ = print "\n[assem]\n"
        val instrs = List.concat(map (Codegen.codegen frame) stms')
        val format = Assem.format saytemp
         in app (fn i => TextIO.output(out, format i)) instrs
        end
    | emitFrag out (F.STRING(lab, s)) =
        TextIO.output(out, Symbol.name lab ^ ": \"" ^ s ^ "\"")
                           (* F.string(lab, s) *)

  fun compile filename = let
        val absyn = parse filename
        val frags = Semant.transProg absyn
        in
          if !ErrorMsg.anyErrors then ()
          else app (emitFrag TextIO.stdOut) frags
        end

end
