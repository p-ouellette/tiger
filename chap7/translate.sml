signature TRANSLATE =
sig
  type level
  type access

  val outermost : level
  val newLevel : {parent: level,
                  name: Temp.label,
                  formals: bool list} -> level
  val formals : level -> access list
  val allocLocal : level -> bool -> access

  type exp

  val simpleVar : access * level -> exp
  val fieldVar : exp * int -> exp
  val subscriptVar : exp * exp -> exp
  val nilExp : exp
  val intExp : int -> exp
  val stringExp : string -> exp
  val callExp : {f: Temp.label,
                 declvl: level,
                 curlvl: level,
                 args: exp list} -> exp
  val opExp : Absyn.oper * Types.ty -> exp * exp -> exp
  val recordExp : exp list -> exp
  val arrayExp : exp * exp -> exp
  val seqExp : exp list -> exp
  val assignExp : exp * exp -> exp
  val ifThenExp : exp * exp -> exp
  val ifExp : exp * exp * exp -> exp
  val whileExp : exp * exp * Temp.label -> exp
  val forExp : access * exp * exp * exp * Temp.label -> exp
  val breakExp : Temp.label -> exp

end

structure Translate : TRANSLATE =
struct
  
  structure A = Absyn
  structure Frame = MIPSFrame
  structure T = Tree
  structure Ty = Types

  val wordSize = Frame.wordSize
  val impossible = ErrorMsg.impossible
  val unimplemented = ErrorMsg.unimplemented

  datatype level = Outermost
                 | Inner of level * Frame.frame * unit ref

  type access = level * Frame.access

  val outermost = Outermost

  (* adds additional formal parameter for static link *)
  fun newLevel {parent, name, formals} =
        Inner(parent, Frame.newFrame {name=name, formals=true::formals}, ref())

  fun sameLevel (Outermost, Outermost) = true
    | sameLevel (Inner(_,_,ref1), Inner(_,_,ref2)) = ref1 = ref2
    | sameLevel _ = false

  fun staticLink Outermost = impossible "staticLink Outermost"
    | staticLink (Inner(_,frame,_)) =
        case Frame.formals frame
          of [] => impossible "missing static link"
           | x::_ => x

  fun formals Outermost = impossible "formals Outermost"
    | formals (l as Inner(_,frame,_)) =
        case Frame.formals frame
          of [] => impossible "missing static link"
           | _::formals => map (fn a => (l,a)) formals

  fun allocLocal Outermost _ = impossible "allocLocal Outermost"
    | allocLocal (l as Inner(_,frame,_)) escape =
        (l, Frame.allocLocal frame escape)

  (* helpers for building IR trees *)

  fun seq [] = impossible "empty seq"
    | seq [x] = x
    | seq (x::xs) = T.SEQ(x, seq xs)

  fun memOffset (base, offset) = T.MEM(T.BINOP(T.PLUS, base, offset))

  (* expression conversion functions *)

  datatype exp = Ex of T.exp
               | Nx of T.stm
               | Cx of Temp.label * Temp.label -> T.stm

  fun unEx (Ex e) = e
    | unEx (Nx(T.EXP e)) = e
    | unEx (Nx _) = impossible "unEx (Nx _)"
    | unEx (Cx genstm) = let
        val r = T.TEMP(Temp.newtemp())
        val t = Temp.newlabel()
        val f = Temp.newlabel()
         in T.ESEQ(seq [T.MOVE(r, T.CONST 1),
                        genstm(t, f),
                        T.LABEL f,
                        T.MOVE(r, T.CONST 0),
                        T.LABEL t],
                   r)
        end

  fun unNx (Nx s) = s
    | unNx (Ex e) = T.EXP e
    | unNx (Cx genstm) = let
        val l = Temp.newlabel()
         in seq [genstm(l, l), T.LABEL l]
        end

  fun unCx (Cx genstm) = genstm
    | unCx (Ex(T.CONST 0)) = (fn (_, f) => T.JUMP(T.NAME f, [f]))
    | unCx (Ex(T.CONST _)) = (fn (t, _) => T.JUMP(T.NAME t, [t]))
    | unCx (Ex e) = (fn (t, f) => T.CJUMP(T.EQ, e, T.CONST 0, f, t))
    | unCx (Nx _) = impossible "unCx(Nx _)"

  (* building IR expressions *)

  (* Returns a Tree.exp that computes the address of the frame at level declvl
   * when the current level is curlvl.
   *)
  fun framePtr (Outermost, _) = impossible "framePtr(Outermost, _)"
    | framePtr (_, Outermost) = impossible "framePtr(_, Outermost)"
    | framePtr (declvl, curlvl) = let
        fun fptr (Outermost, _) = impossible "could not find declared level"
          | fptr (l as Inner(parent,_,_), exp) =
              if sameLevel(l, declvl) then exp
              else fptr(parent, Frame.expOfAccess (staticLink l) exp)
         in fptr (curlvl, T.TEMP Frame.FP)
        end

  fun simpleVar (_, Outermost) = impossible "simpleVar(_, Outermost)"
    | simpleVar ((declvl, access), curlvl) =
        Ex(Frame.expOfAccess access (framePtr(declvl, curlvl)))

  fun fieldVar (exp, i) =
        Ex(memOffset(unEx exp, T.CONST(i * wordSize)))

  fun subscriptVar (exp, iexp) = let
        val offset = T.BINOP(T.MUL, unEx iexp, T.CONST wordSize)
         in Ex(memOffset(unEx exp, offset))
        end

  val nilExp = Ex(T.CONST 0)

  fun intExp i = Ex(T.CONST i)

  (* TODO *)
  fun stringExp s = let
        val l = Temp.newlabel()
         in Ex(T.NAME l)
        end

  fun callExp {f, declvl, curlvl, args} = let
        val sl = framePtr(declvl, curlvl)
         in Ex(T.CALL(T.NAME f, sl :: map unEx args))
        end

  fun arith oper (lt, rt) = Ex(T.BINOP(oper, unEx lt, unEx rt))

  fun cmpString oper (lt, rt) = let
        fun id (x, y) = (x, y)
        fun sw (x, y) = (y, x)
        val test = Frame.externalCall("stringEqual", [unEx lt, unEx rt])
        val labsw =
          case oper
            of T.EQ => id
             | T.NE => sw
             | T.LT => unimplemented()
             | T.LE => unimplemented()
             | T.GT => unimplemented()
             | T.GE => unimplemented()
             | _ => impossible "bad comparison operator"
        fun genstm (t, f) = T.CJUMP(T.EQ, test, T.CONST 0, f, t)
         in Cx(genstm o labsw)
        end

  fun cmpScalar oper (lt, rt) =
        Cx(fn (t, f) => T.CJUMP(oper, unEx lt, unEx rt, t, f))

  fun opExp (A.PlusOp, _)   = arith T.PLUS
    | opExp (A.MinusOp, _)  = arith T.MINUS
    | opExp (A.TimesOp, _)  = arith T.MUL
    | opExp (A.DivideOp, _) = arith T.DIV
    | opExp (A.EqOp,  Ty.STRING) = cmpString T.EQ
    | opExp (A.NeqOp, Ty.STRING) = cmpString T.NE
    | opExp (A.LtOp,  Ty.STRING) = cmpString T.LT
    | opExp (A.LeOp,  Ty.STRING) = cmpString T.LE
    | opExp (A.GtOp,  Ty.STRING) = cmpString T.GT
    | opExp (A.GeOp,  Ty.STRING) = cmpString T.GE
    | opExp (A.EqOp, _)  = cmpScalar T.EQ
    | opExp (A.NeqOp, _) = cmpScalar T.NE
    | opExp (A.LtOp, _)  = cmpScalar T.LT
    | opExp (A.LeOp, _)  = cmpScalar T.LE
    | opExp (A.GtOp, _)  = cmpScalar T.GT
    | opExp (A.GeOp, _)  = cmpScalar T.GE

  fun recordExp fields = let
        val size = length fields * wordSize
        val base = Frame.externalCall("allocRecord", [T.CONST size])
        val r = T.TEMP(Temp.newtemp())
        fun moveField (field, (moves, offset)) = let
              val move = T.MOVE(memOffset(r, T.CONST offset), unEx field)
               in (move::moves, offset + wordSize)
              end
        val (moves, _) = foldl moveField ([], 0) fields
         in Ex(T.ESEQ(seq (T.MOVE(r, base) :: moves), r))
        end

  fun arrayExp (size, init) =
        Ex(Frame.externalCall("initArray", [unEx size, unEx init]))

  fun seqExp [] = Nx(T.EXP(T.CONST 0))
    | seqExp [x] = x
    | seqExp (x::xs) = Ex(T.ESEQ(unNx x, unEx(seqExp xs)))

  fun assignExp (var, exp) = Nx(T.MOVE(unEx var, unEx exp))

  fun ifThenExp (test, texp) = let
        val t = Temp.newlabel()
        val f = Temp.newlabel()
        val testgen = unCx test
        val tstm = unNx texp
         in Nx(seq [testgen(t, f), T.LABEL t, tstm, T.LABEL f])
        end

  fun ifCondBool (test, tgen, fb) = let
        val z = Temp.newlabel()
        val testgen = unCx test
         in Cx(fn (t, f) => seq [testgen(z, if fb then t else f),
                                 T.LABEL z,
                                 tgen(t, f)])
        end

  fun ifBoolCond (test, tb, fgen) = let
        val z = Temp.newlabel()
        val testgen = unCx test
         in Cx(fn (t, f) => seq [testgen(if tb then t else f, z),
                                 T.LABEL z,
                                 fgen(t, f)])
        end

  fun ifExp (test, Cx tgen, Ex(T.CONST 0)) = ifCondBool(test, tgen, false)
    | ifExp (test, Cx tgen, Ex(T.CONST 1)) = ifCondBool(test, tgen, true)
    | ifExp (test, Ex(T.CONST 0), Cx fgen) = ifBoolCond(test, false, fgen)
    | ifExp (test, Ex(T.CONST 1), Cx fgen) = ifBoolCond(test, true, fgen)
    | ifExp (test, Cx tgen, Cx fgen) = let
        val y = Temp.newlabel()
        val z = Temp.newlabel()
        val testgen = unCx test
         in Cx(fn (t, f) => seq [testgen(y, z),
                                 T.LABEL y, tgen(t, f),
                                 T.LABEL z, fgen(t, f)])
        end
    | ifExp (test, Nx tstm, Nx fstm) = let
        val t = Temp.newlabel()
        val f = Temp.newlabel()
        val j = Temp.newlabel()
        val testgen = unCx test
         in Nx(seq [testgen(t, f),
                    T.LABEL t, tstm,
                    T.JUMP(T.NAME j, [j]),
                    T.LABEL f,
                    fstm,
                    T.LABEL j])
        end
    | ifExp (test, Ex texp, Ex fexp) = let
        val r = T.TEMP(Temp.newtemp())
        val t = Temp.newlabel()
        val f = Temp.newlabel()
        val j = Temp.newlabel()
        val testgen = unCx test
         in Ex(T.ESEQ(seq [testgen(t, f),
                           T.LABEL t,
                           T.MOVE(r, texp),
                           T.JUMP(T.NAME j, [j]),
                           T.LABEL f,
                           T.MOVE(r, fexp),
                           T.LABEL j],
                      r))
        end
    | ifExp _ = impossible "bad if expression"

  fun whileExp (test, body, donelab) = let
        val testlab = Temp.newlabel()
        val bodylab = Temp.newlabel()
        val testgen = unCx test
         in Nx(seq [T.LABEL testlab,
                    testgen(bodylab, donelab),
                    T.LABEL bodylab,
                    unNx body,
                    T.JUMP(T.NAME testlab, [testlab]),
                    T.LABEL donelab])
        end

  fun forExp ((_, access), lo, hi, body, donelab) = let
        val var = Frame.expOfAccess access (T.TEMP Frame.FP)
        val testlab = Temp.newlabel()
        val looplab = Temp.newlabel()
         in Nx(seq [T.MOVE(var, unEx lo),
                    T.JUMP(T.NAME testlab, [testlab]),
                    T.LABEL looplab,
                    unNx body,
                    T.MOVE(var, T.BINOP(T.PLUS, var, T.CONST 1)),
                    T.LABEL testlab,
                    T.CJUMP(T.LT, var, unEx hi, looplab, donelab)])
        end

  fun breakExp donelab = Nx(T.JUMP(T.NAME donelab, [donelab]))

end
