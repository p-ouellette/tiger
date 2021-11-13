signature TABLE =
sig
  type key
  type 'a table
  val empty : 'a table
  val enter : 'a table * key * 'a -> 'a table
  val find  : 'a table * key -> 'a option
  val lookup : 'a table * key -> 'a
end
