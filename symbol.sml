signature SYMBOL =
sig
  eqtype symbol
  val symbol : string -> symbol
  val name : symbol -> string
  val eq : symbol * symbol -> bool
  type 'a table
  val empty : 'a table
  val enter : 'a table * symbol * 'a -> 'a table
  val find : 'a table * symbol -> 'a option
  val look  : 'a table * symbol -> 'a
end

structure Symbol :> SYMBOL =
struct
  type symbol = string * int

  structure H = HashTable

  exception Symbol
  val nextsym = ref 0
  val sizeHint = 128
  val hashtable : (string, int) H.hash_table =
        H.mkTable(HashString.hashString, op = ) (sizeHint, Symbol)

  fun symbol name =
        case H.find hashtable name
          of SOME i => (name, i)
           | NONE => let val i = !nextsym
                      in nextsym := i + 1;
                         H.insert hashtable (name, i);
                         (name, i)
                     end

  fun name(s, n) = s

  fun eq((_,n1): symbol, (_,n2): symbol) = n1 = n2

  structure Table = IntMapTable(type key = symbol
                                fun getInt(s, n) = n)

  type 'a table= 'a Table.table
  val empty = Table.empty
  val enter = Table.enter
  val find = Table.find
  val look = Table.lookup
end
