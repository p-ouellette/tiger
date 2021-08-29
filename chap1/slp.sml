type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp
val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

(* part 1 *)

fun maxargs (CompoundStm(stm1,stm2)) = Int.max(maxargs stm1, maxargs stm2)
  | maxargs (AssignStm(_,exp)) = maxargsExp exp
  | maxargs (PrintStm explist) =
      foldl Int.max (length explist) (map maxargsExp explist)

and maxargsExp (OpExp(exp1,_,exp2)) = Int.max(maxargsExp exp1, maxargsExp exp2)
  | maxargsExp (EseqExp(stm,exp))   = Int.max(maxargs stm, maxargsExp exp)
  | maxargsExp _ = 0

(* part 2 *)

fun update (tab, id, v) = (id,v)::tab

fun lookup ([], id)            = 0
  | lookup ((i,v)::tab, id:id) = if i = id then v else lookup(tab, id)

fun interpStm (CompoundStm(stm1,stm2), tab) =
      interpStm(stm2, interpStm(stm1, tab))
  | interpStm (AssignStm(id,exp), tab) =
      let val (v, tab) = interpExp(exp, tab)
      in update(tab, id, v) end
  | interpStm (PrintStm([]), tab) = (print "\n"; tab)
  | interpStm (PrintStm(exp::explist), tab) =
      let val (v, tab) = interpExp(exp, tab)
      in (print ((Int.toString v) ^ " ");
          interpStm(PrintStm(explist), tab))
      end

and interpExp (IdExp id, tab) = (lookup(tab, id), tab)
  | interpExp (NumExp v, tab) = (v, tab)
  | interpExp (OpExp(exp1,binop,exp2), tab) =
      let val (v1, tab) = interpExp(exp1, tab)
          val (v2, tab) = interpExp(exp2, tab)
      in case binop of
              Plus  => (v1 + v2, tab)
            | Minus => (v1 - v2, tab)
            | Times => (v1 * v2, tab)
            | Div   => (v1 div v2, tab)
      end
  | interpExp (EseqExp(stm,exp), tab) =
      interpExp(exp, interpStm(stm, tab))

fun interp stm = (interpStm(stm, []); ())
