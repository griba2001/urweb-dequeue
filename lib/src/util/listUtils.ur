(* List utils. *)

structure L = List

(* listSplitLast
   @param li
   @return None | Some (init li, last li)
 *)

fun listSplitLast[a] (li: list a): option (list a * a) =
    let splitLast' [] li
    where
      fun splitLast' (acc: list a) (li: list a) =
      case li of
        | x :: [] => Some (L.rev acc, x)
        | x :: rest => splitLast' (x :: acc) rest
        | [] => None
    end

(* 
   @param from
   @param n number of elements
   @return list of numbers
 *)

fun rangeList (from: int) (n: int): list int = (* n >= 0 *)
   let range' [] n
   where
     fun range' (acc: list int) (i: int) =
           if i = 0
             then acc
             else range' ((from + i - 1) :: acc) (i - 1)
   end

fun listSpan[a] (prop: a -> bool) (li: list a): list a * list a =
   let span' [] li
   where
     fun span' (acc: list a) (li': list a) =
         case li' of
           | x :: xs => if prop x
                            then span' (x :: acc) xs
                            else (L.rev acc, li')
           | [] => (L.rev acc, li')
   end

fun listPartition[a] (prop: a -> bool) (li: list a): list a * list a =
     let part' [] [] li
     where
       fun part' (pos_acc: list a) (neg_acc: list a) (li': list a) =
          case li' of
            | x :: xs => if prop x
                            then part' (x :: pos_acc) neg_acc xs
                            else part' pos_acc (x :: neg_acc) xs
            | [] => (L.rev pos_acc, L.rev neg_acc)
     end
