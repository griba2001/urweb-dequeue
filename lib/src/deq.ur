(* splitted list dequeue *)

structure L = List
structure O = Option  (* import option eq instance *)

datatype t a = Deq of (list a * list a)

fun cons[a]: a -> t a -> t a = fn x (Deq (l, r)) => Deq (x :: l, r)
fun snoc[a]: a -> t a -> t a = fn x (Deq (l, r)) => Deq (l, x :: r)

fun fromList[a]: list a -> t a = fn li =>
     case li of
         [] => Deq ([], [])
         | _ => let val (prefix, sufix) = L.splitAt (L.length li `divide` 2) li
                in Deq (prefix, L.rev sufix)
                end   

val toList[a]: t a -> list a = fn (Deq (l, r)) => l `L.append` (L.rev r)

(* internal listSplitLast
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

fun viewL[a]: t a -> option (a * t a) = fn (Deq (l, r)) =>
   case l of
      | x :: xs => Some (x, Deq (xs, r))
      | [] => (case listSplitLast r of
               | None => None
               | Some (ys, y) => Some (y, fromList (L.rev ys))
               )

fun viewR[a]: t a -> option (t a * a) = fn (Deq (l, r)) =>
    case r of
      | x :: xs => Some (Deq (l, xs), x)
      | [] => (case listSplitLast l of
               | None => None
               | Some (ys, y) => Some (fromList ys, y)
               )

(* monoid ops *)

val empty[a]: t a = Deq ([], [])

fun append[a]: t a -> t a -> t a = fn (Deq (l1, r1)) (Deq (l2, r2)) =>
   let
     Deq (prefix, r2)
   where
     val prefix = l1 `L.append` L.revAppend r1 l2
   end

(* query ops *)

val null[a]: t a -> bool = fn (Deq (l, r)) =>
    case (l, r) of
      ([], []) => True
      | _ => False

val size[a]: t a -> int = fn (Deq (l, r)) => L.length l + L.length r

fun member[a] (_ : eq a) (x: a): t a -> bool = fn (Deq (l, r)) => L.mem x l || L.mem x r

fun nth[a]: t a -> int -> option a = fn (Deq (l, r)) i =>
    if i < 0 then None else
    let val len_l = L.length l
    in if i < len_l
            then L.nth l i
            else let L.nth r rev_sufix_idx
                 where
                    val sufix_idx = i - len_l
                    val rev_sufix_idx = L.length r -1 - sufix_idx
                 end
    end

fun find[a] (prop: a -> bool): t a -> option a = fn (Deq (l, r)) =>
    case L.find prop l of
      Some x => Some x
      | None => L.find prop r

(* class instances *)

val eq_pair[a][b] (_:eq a) (_:eq b): eq (a * b) =
   let fun eq' (p: a * b) (q: a * b) = p.1 = q.1 && p.2 = q.2
   in mkEq eq'
   end

val eq_deq[a] (_:eq a): eq (t a) = let fun eq' (d1: t a) (d2: t a) = toList d1 = toList d2
                            in mkEq eq'
                            end

val show_deq[a](_:show a): show (t a) = let fun show' (d1: t a) = show (toList d1)
                                          in mkShow show'
                                          end       
                                

(* map/filter/fold ops *)

val mp[a][b]: (a -> b) -> t a -> t b = fn f (Deq (l, r)) =>

     Deq (L.mp f l, L.mp f r)

val mapPartial[a][b]: (a -> option b) -> t a -> t b = fn f (Deq (l, r)) =>

     Deq (L.mapPartial f l, L.mapPartial f r)

val filter[a]: (a -> bool) -> t a -> t a = fn prop (Deq (l, r)) =>

     Deq (L.filter prop l, L.filter prop r)

val foldl[a][b]: (a -> b -> b) -> b -> t a -> b = fn binop z (Deq (l, r)) =>
     let
        val acc = L.foldl binop z l
     in
        L.foldl binop acc (L.rev r)
     end

val foldr[a][b]: (a -> b -> b) -> b -> t a -> b = fn binop z (Deq (l, r)) =>
     let
        val acc = L.foldl binop z r
     in
        L.foldr binop acc l 
     end

(* fun withPartialBinop:
   converts partial foldings' function (a -> b -> option b) to (a -> b -> b) *)

fun withPartialBinop[a][b] (f: a -> b -> option b) (x: a) (acc: b): b =
    case f x acc of
      None => acc
      | Some res => res

(* fun withFilterAndBinop
   converts filter and binop to binop *)
fun withFilterAndBinop[a][b] (prop: a -> bool) (f: a -> b -> b) (x: a) (acc: b): b =
     if prop x
        then f x acc
        else acc 

val foldlPartial[a][b]: (a -> b -> option b) -> b -> t a -> b = fn partialBinop z deq =>

      foldl (withPartialBinop partialBinop) z deq

val foldrPartial[a][b]: (a -> b -> option b) -> b -> t a -> b = fn partialBinop z deq =>

      foldr (withPartialBinop partialBinop) z deq

val filterFoldl[a][b]: (a -> bool) -> (a -> b -> b) -> b -> t a -> b = fn prop binop =>

    foldl (withFilterAndBinop prop binop)

val filterFoldr[a][b]: (a -> bool) -> (a -> b -> b) -> b -> t a -> b = fn prop binop =>

    foldr (withFilterAndBinop prop binop)

val foldlAccum[a][b][c]: (a -> b -> c * b) -> b -> t a -> t c * b = fn stateOp ini (Deq (l, r)) =>
     let
         val (list_lRes, st) = L.foldlMap stateOp ini l
         val (list_rRes, st) = L.foldlMap stateOp st (L.rev r)
     in
         (Deq (list_lRes, L.rev list_rRes), st)
     end

(* specific folds *)

fun sum[a] (_:num a): t a -> a = fn (Deq (l, r)) =>
   let list_sum l + list_sum r
   where
     val list_sum: list a -> a = L.foldl plus zero
   end

val float_prod: t float -> float = fn (Deq (l, r)) =>
   let list_prod l * list_prod r
   where
     val list_prod: list float -> float = L.foldl times 1.0
   end

val all[a]: (a -> bool) -> t a -> bool = fn prop (Deq (l, r)) => L.all prop l && L.all prop r

val any[a]: (a -> bool) -> t a -> bool = fn prop (Deq (l, r)) => L.exists prop l || L.exists prop r

(* invariants *)

fun propConsViewL[a] (_:eq a) (x: a) (d1: t a): bool = viewL (cons x d1) = Some (x, d1)

fun propSnocViewR[a] (_:eq a) (x: a) (d1: t a): bool = viewR (snoc x d1) = Some (d1, x)

fun propFromToList[a] (_:eq a) (li: list a): bool = li = toList ( fromList li)

fun range (from: int) (n: int): list int =
   let range' [] 0
   where
     fun range' (acc: list int) (i: int) = if i = n then acc else range' ((from + i) :: acc) (i + 1)
   end

fun propNthSameElements[a] (_:eq a) (d1: t a): bool =
   let val li = toList d1
       val idxs = range 0 (size d1)
       val deqItems = L.mp (nth d1) idxs
       val listItems = L.mp (L.nth li) idxs 
   in
      deqItems = listItems
   end