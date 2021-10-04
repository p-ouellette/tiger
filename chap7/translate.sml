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

end

structure Translate : TRANSLATE =
struct
  
  structure Frame = MIPSFrame
  structure T = Tree

  val impossible = ErrorMsg.impossible

  datatype level = Outermost
                 | Inner of level * Frame.frame

  type access = level * Frame.access

  val outermost = Outermost

  (* adds additional formal parameter for static link *)
  fun newLevel {parent, name, formals} =
        Inner(parent, Frame.newFrame {name=name, formals=true::formals})

  fun formals Outermost = impossible "formals Outermost"
    | formals (l as Inner(_,frame)) =
        case Frame.formals frame
          of [] => impossible "missing static link"
           | _::formals => map (fn a => (l,a)) formals

  fun allocLocal Outermost _ = impossible "allocLocal Outermost"
    | allocLocal (l as Inner(_,frame)) escape =
        (l, Frame.allocLocal frame escape)

  (* expression conversion functions *)

  datatype exp = Ex of Tree.exp
               | Nx of Tree.stm
               | Cx of Temp.label * Temp.label -> Tree.stm

  fun seq [] = T.EXP(T.CONST 0)
    | seq [x] = x
    | seq (x::xs) = T.SEQ(x, seq xs)

  fun unEx (Ex e) = e
    | unEx (Nx s) = T.ESEQ(s, T.CONST 0)
    | unEx (Cx genstm) = let
        val r = Temp.newtemp()
        val t = Temp.newlabel()
        val f = Temp.newlabel()
         in T.ESEQ(seq [T.MOVE(T.TEMP r, T.CONST 1),
                        genstm(t, f),
                        T.LABEL f,
                        T.MOVE(T.TEMP r, T.CONST 0),
                        T.LABEL t],
                   T.TEMP r)
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
    | unCx (Nx _) = impossible "unCx(Nx s)"

end
