functor IntMapTable (type key
                     val getInt: key -> int) : TABLE =
struct
  type key = key
  type 'a table = 'a IntBinaryMap.map
  val empty = IntBinaryMap.empty
  fun enter (t, k, a) = IntBinaryMap.insert(t, getInt k, a)
  fun find (t, k) = IntBinaryMap.find(t, getInt k)
  fun lookup (t, k) = IntBinaryMap.lookup(t, getInt k)
  val foldl = IntBinaryMap.foldl
end
