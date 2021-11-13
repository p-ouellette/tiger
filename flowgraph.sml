signature FLOW =
sig
  datatype flowgraph = FGRAPH of {control: Graph.graph,
                                  def: Temp.Set.set Graph.Table.table,
                                  use: Temp.Set.set Graph.Table.table,
                                  ismove: bool Graph.Table.table}

  (* Note:  any "use" within the block is assumed to be BEFORE a "def" 
        of the same variable.  If there is a def(x) followed by use(x)
       in the same block, do not mention the use in this data structure,
       mention only the def.

     More generally:
       If there are any nonzero number of defs, mention def(x).
       If there are any nonzero number of uses BEFORE THE FIRST DEF,
           mention use(x).

     For any node in the graph,  
           Graph.Table.lookup(def,node) = SOME(def-set)
           Graph.Table.lookup(use,node) = SOME(use-set)
   *)

end

structure Flow : FLOW =
struct
  datatype flowgraph = FGRAPH of {control: Graph.graph,
                                  def: Temp.Set.set Graph.Table.table,
                                  use: Temp.Set.set Graph.Table.table,
                                  ismove: bool Graph.Table.table}
end
