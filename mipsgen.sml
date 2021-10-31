structure MIPSGen : CODEGEN =
struct

  structure A = Assem
  structure Frame = MIPSFrame
  structure T = Tree

  val impossible = ErrorMsg.impossible
  val unimplemented = ErrorMsg.unimplemented

  fun opInstr T.PLUS  = "add"
    | opInstr T.MINUS = "sub"
    | opInstr T.MUL   = "mul"
    | opInstr T.DIV   = "div"
    | opInstr _ = unimplemented()

  fun codegen frame stm = let
        val ilist = ref ([]: A.instr list)
        fun emit x = ilist := x :: !ilist
        fun result gen = let val t = Temp.newtemp() in gen t; t end
        fun imm i =
              if i < 0 then "-" ^ Int.toString(Int.abs i)
              else Int.toString i

            (* load word *)
        fun munchStm (T.MOVE(T.TEMP t, T.MEM(T.BINOP(T.PLUS, e, T.CONST i)))) =
              munchLW(t, e, i)
          | munchStm (T.MOVE(T.TEMP t, T.MEM(T.BINOP(T.PLUS, T.CONST i, e)))) =
              munchLW(t, e, i)
          | munchStm (T.MOVE(T.TEMP t, T.MEM(T.CONST i))) =
              emit(A.OPER{assem="lw `d0, " ^ imm i,
                          src=[], dst=[t], jump=NONE})
          | munchStm (T.MOVE(T.TEMP t, T.MEM(e))) =
              emit(A.OPER{assem="lw `d0, (`s0)",
                          src=[munchExp e], dst=[t], jump=NONE})
            (* load immediate *)
          | munchStm (T.MOVE(T.TEMP t, T.CONST i)) =
              emit(A.OPER{assem="li `d0, " ^ imm i,
                          src=[], dst=[t], jump=NONE})
            (* load address *)
          | munchStm (T.MOVE(T.TEMP t, T.BINOP(T.PLUS, e, T.CONST i))) =
              munchLA(t, e, i)
          | munchStm (T.MOVE(T.TEMP t, T.BINOP(T.PLUS, T.CONST i, e))) =
              munchLA(t, e, i)
            (* move *)
          | munchStm (T.MOVE(T.TEMP t, e)) =
              emit(A.OPER{assem="move `d0, `s0",
                          src=[munchExp e], dst=[t], jump=NONE})
            (* store word *)
          | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) =
              munchSW(e1, i, e2)
          | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
              munchSW(e1, i, e2)
          | munchStm (T.MOVE(T.MEM(T.CONST i), e)) =
              emit(A.OPER{assem="sw `s0, " ^ imm i,
                          src=[munchExp e], dst=[], jump=NONE})
          | munchStm (T.MOVE(T.MEM(e1), e2)) =
              emit(A.OPER{assem="sw `s0, (`s1)",
                          src=[munchExp e2, munchExp e1], dst=[], jump=NONE})

          | munchStm (T.JUMP(T.NAME l, _)) =
              emit(A.OPER{assem="j `j0",
                          src=[], dst=[], jump=SOME [l]})

          | munchStm (T.EXP e) = (munchExp e; ())

          | munchStm (T.LABEL lab) =
              emit(A.LABEL{assem=Symbol.name lab ^ ":", lab=lab})

          | munchStm _ = impossible "bad tree (munchStm)"

        and munchExp (T.BINOP(oper, e, T.CONST i)) = munchArithi(oper, e, i)
          | munchExp (T.BINOP(oper, T.CONST i, e)) = munchArithi(oper, e, i)
          | munchExp (T.BINOP(oper, e1, e2)) =
              result(fn r => emit(A.OPER
                      {assem=opInstr oper ^ " `d0, `s0, `s1",
                       src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))

          | munchExp (T.CALL(T.NAME f, args)) =
              (emit(A.OPER{assem="jal `j0",
                           src=munchArgs(0, args),
                           dst=Frame.calldefs,
                           jump=SOME [f]});
               Frame.RV)

          | munchExp (T.TEMP t) = t

          | munchExp _ = impossible "bad tree (munchExp)"

        and munchLW (t, e, i) =
              emit(A.OPER{assem="lw `d0, " ^ imm i ^ "(`s0)",
                          src=[munchExp e], dst=[t], jump=NONE})

        and munchLA (t, e, i) =
              emit(A.OPER{assem="la `d0, " ^ imm i ^ "(`s0)",
                          src=[munchExp e], dst=[t], jump=NONE})

        and munchSW (e1, i, e2) =
              emit(A.OPER{assem="sw `s0, " ^ imm i ^ "(`s1)",
                          src=[munchExp e2, munchExp e1], dst=[], jump=NONE})

        and munchArithi (oper, e, i) =
              result(fn r => emit(A.OPER
                      {assem=opInstr oper ^ " `d0, `s0, " ^ imm i,
                       src=[munchExp e], dst=[r], jump=NONE}))

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
