signature FRAME =
sig

  type frame
  type access

  val newFrame : {name: Temp.label, formals: bool list} -> frame
  val name : frame -> Temp.label
  val formals : frame -> access list
  val allocLocal : frame -> bool -> access

  type register = string
  val registers : register list
  val tempMap : register Temp.Table.table

  val SP : Temp.temp
  val FP : Temp.temp
  val RV : Temp.temp
  val ZERO : Temp.temp

  (* Registers "trashed" by call. *)
  val calldefs : Temp.temp list

  val wordSize : int

  datatype frag = PROC of {frame: frame, body: Tree.stm}
                | STRING of Temp.label * string

  (* Returns a Tree expression for an access.
   * The Tree.exp argument is the frame pointer for the relevant frame.
   *)
  val expOfAccess : access -> Tree.exp -> Tree.exp

  val externalCall : string * Tree.exp list -> Tree.exp

  (* Returns the correct position (register or memory) for the ith argument
   * before a call.
   *)
  val argPos : int -> Tree.exp

  (* View shift *)
  val procEntryExit1 : frame * Tree.stm -> Tree.stm

  (* Appends a "sink" instruction to the function body to tell the register
   * allocator that certain registers are live at procedure exit.
   *)
  val procEntryExit2 : frame * Assem.instr list -> Assem.instr list

  val procEntryExit3 : frame * Assem.instr list -> {prolog: string,
                                                    body: Assem.instr list,
                                                    epilog: string}

  (* Returns a string containing the assembly-language instructions required to
   * define a string literal.
   *)
  val string : Temp.label * string -> string

end
