(* make this an abstraction sometime *)
structure Temp : TEMP =
struct
  type temp = int
  val temps = ref 100
  fun newtemp() = let val t = !temps in temps := t+1; t end

  fun makestring t = "t" ^ Int.toString t
  val compare = Int.compare

  structure Table = IntMapTable(type key = int
                                fun getInt n = n)

  structure TempKey : ORD_KEY =
    struct
      type ord_key = temp
      val compare = compare
    end
  structure Set = RedBlackSetFn(TempKey)

  type label = Symbol.symbol

  local
    structure F = Format
    fun postinc x = let val i = !x in x := i+1; i end
    val labs = ref 0
  in
    fun newlabel() = Symbol.symbol(F.format "L%d" [F.INT(postinc labs)])
    val namedlabel = Symbol.symbol
  end

end
