structure MIPSGen : CODEGEN =
struct

  structure A = Assem
  structure Frame = MIPSFrame
  structure T = Tree

  val impossible = ErrorMsg.impossible

  fun binop T.PLUS  = "add"
    | binop T.MINUS = "sub"
    | binop T.MUL   = "mul"
    | binop T.DIV   = "div"
    | binop T.AND   = "and"
    | binop T.OR    = "or"
    | binop T.XOR   = "xor"
    | binop T.LSHIFT  = "sll"
    | binop T.RSHIFT  = "srl"
    | binop T.ARSHIFT = "sra"

  fun relop T.EQ = "beq"
    | relop T.NE = "bne"
    | relop T.LT = "blt"
    | relop T.GT = "bgt"
    | relop T.LE = "ble"
    | relop T.GE = "bge"
    | relop T.ULT = "bltu"
    | relop T.ULE = "bleu"
    | relop T.UGT = "bgtu"
    | relop T.UGE = "bgeu"

  fun codegen frame stm = let
        val ilist = ref ([]: A.instr list)
        fun emit x = ilist := x :: !ilist
        fun result gen = let val t = Temp.newtemp() in gen t; t end
        fun imm i =
              if i < 0 then "-" ^ Int.toString(Int.abs i)
              else Int.toString i
        val lab = Symbol.name

        fun munchStm (T.MOVE(T.TEMP t, T.MEM addr)) = munchLW addr t
          | munchStm (T.MOVE(T.TEMP t, T.NAME l))  = munchLA l t
          | munchStm (T.MOVE(T.TEMP t, T.CONST i)) = munchLI i t
          | munchStm (T.MOVE(T.TEMP t, T.BINOP b)) = munchBinop b t
          | munchStm (T.MOVE(T.TEMP t, e)) =
              emit(A.MOVE{assem="move `d0, `s0\n", src=munchExp e, dst=t})
          | munchStm (T.MOVE(T.MEM addr, e)) = munchSW(addr, e)
          | munchStm (T.CJUMP b) = munchBranch b
          | munchStm (T.JUMP(T.NAME l, _)) =
              emit(A.OPER{assem="j `j0\n", src=[], dst=[], jump=SOME [l]})
          | munchStm (T.LABEL l) =
              emit(A.LABEL{assem=lab l ^ ":\n", lab=l})
          | munchStm (T.EXP e) = (munchExp e; ())
          | munchStm s = (PrintTree.printTree(TextIO.stdOut, s);
                          impossible "bad tree (munchStm)")

        and munchExp (T.MEM e) = result(munchLW e)
          | munchExp (T.NAME l) = result(munchLA l)
          | munchExp (T.CONST 0) = Frame.ZERO
          | munchExp (T.CONST i) = result(munchLI i)
          | munchExp (T.BINOP b) = result(munchBinop b)
          | munchExp (T.CALL(T.NAME f, args)) =
              (emit(A.OPER{assem="jal `j0\n",
                           src=munchArgs(0, args),
                           dst=Frame.calldefs,
                           jump=SOME [f]});
               Frame.RV)
          | munchExp (T.TEMP t) = t
          | munchExp e = (PrintTree.printTree(TextIO.stdOut, T.EXP e);
                          impossible "bad tree (munchExp)")

        and munchLW addr t = let
              val (addr, src) =
                case addr
                  of T.BINOP(T.PLUS, e, T.CONST i) => (imm i ^ "(`s0)", [e])
                   | T.BINOP(T.PLUS, T.CONST i, e) => (imm i ^ "(`s0)", [e])
                   | T.CONST i => (imm i, [])
                   | e => ("(`s0)", [e])
              in
                emit(A.OPER{assem="lw `d0, " ^ addr ^ "\n",
                            src=map munchExp src, dst=[t],
                            jump=NONE})
              end

        and munchSW (addr, e) = let
              val (addr, src) =
                case addr
                  of T.BINOP(T.PLUS, e', T.CONST i) => (imm i ^ "(`s1)", [e'])
                   | T.BINOP(T.PLUS, T.CONST i, e') => (imm i ^ "(`s1)", [e'])
                   | T.CONST i => (imm i, [])
                   | e' => ("(`s1)", [e'])
              in
                emit(A.OPER{assem="sw `s0, " ^ addr ^ "\n",
                            src=map munchExp (e::src), dst=[],
                            jump=NONE})
              end

        and munchLA l t =
              emit(A.OPER{assem="la `d0, " ^ lab l ^ "\n",
                          src=[], dst=[t], jump=NONE})

        and munchLI i t =
              emit(A.OPER{assem="li `d0, " ^ imm i ^ "\n",
                          src=[], dst=[t], jump=NONE})

        and munchBinop (oper, e1, e2) t = let
              val (s1, src) =
                case (e1, e2)
                  of (T.CONST i, _) => (imm i, [])
                   | (_, T.CONST i) => (imm i, [])
                   | _ => ("`s1", [e2])
              in
                emit(A.OPER{assem=binop oper ^ " `d0, `s0, " ^ s1 ^ "\n",
                            src=map munchExp (e1::src), dst=[t],
                            jump=NONE})
              end

        and munchBranch (oper, e1, e2, t, f) = let
              val (s1, src) =
                case e2
                  of T.CONST i => (imm i, [])
                   | _ => ("`s1", [e2])
              in
                emit(A.OPER{assem=relop oper ^ " `s0, " ^ s1 ^ ", `j0\n",
                            src=map munchExp (e1::src), dst=[],
                            jump=SOME [t, f]})
              end

        and munchArgs (n, []) = []
          | munchArgs (n, x::xs) = let
              val dst = Frame.argPos n
              val _ = munchStm(T.MOVE(dst, x))
              val arg = munchExp dst
               in arg :: munchArgs(n + 1, xs)
              end

         in munchStm stm;
            rev(!ilist)
        end

end
