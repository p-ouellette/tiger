signature TEMP =
sig
  eqtype temp
  val newtemp : unit -> temp
  val makestring : temp -> string
  val compare : temp * temp -> order
  structure Table : TABLE sharing type Table.key = temp
  structure Set : ORD_SET

  type label = Symbol.symbol
  val newlabel : unit -> label
  val namedlabel : string -> label
end
