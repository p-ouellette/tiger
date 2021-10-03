signature TRANSLATE =
sig
  type level
  type access

  val outermost : level
  val newLevel : {parent: level,
                  name: Temp.label,
                  formals: bool list} -> level
  val formals: level -> access list
  val allocLocal: level -> bool -> access
end

structure Translate : TRANSLATE =
struct
  
  structure Frame = MIPSFrame

  val impossible = ErrorMsg.impossible

  datatype level = Outermost
                 | Inner of level * Frame.frame

  type access = level * Frame.access

  val outermost = Outermost

  (* adds additional formal parameter for static link *)
  fun newLevel {parent, name, formals} =
        Inner(parent, Frame.newFrame {name=name, formals=true::formals})

  fun formals Outermost = impossible "get formals of outermost level"
    | formals (l as Inner(_,frame)) =
        case Frame.formals frame
          of [] => impossible "missing static link"
           | _::formals => map (fn a => (l,a)) formals

  fun allocLocal Outermost _ = impossible "allocLocal in outermost level"
    | allocLocal (l as Inner(_,frame)) escape =
        (l, Frame.allocLocal frame escape)

end
