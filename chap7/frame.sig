signature FRAME =
sig

  type frame
  type access

  val newFrame : {name: Temp.label, formals: bool list} -> frame
  val name : frame -> Temp.label
  val formals : frame -> access list
  val allocLocal : frame -> bool -> access

  val FP : Temp.temp
  val wordSize : int

  (* Returns a Tree expression for an access.
   * The Tree.exp argument is the frame pointer for the relevant frame.
   *)
  val expOfAccess : access -> Tree.exp -> Tree.exp

  val externalCall : string * Tree.exp list -> Tree.exp

end
