(* split list dequeue *)

structure L = List
structure O = Option  (* import option eq instance *)
structure LU = ListUtils
structure PU = PairUtils
structure FU = FunUtils

datatype t a = Deq of (list a * list a)

fun cons[a]: a -> t a -> t a = fn x (Deq (l, r)) => Deq (x :: l, r)
fun snoc[a]: t a -> a -> t a = fn (Deq (l, r)) x => Deq (l, x :: r)

fun fromList[a]: list a -> t a = fn li =>
     case li of
         [] => Deq ([], [])
         | _ => let val (prefix, suffix) = L.splitAt (L.length li `divide` 2) li
                in Deq (prefix, L.rev suffix)
                end   

val toList[a]: t a -> list a = fn (Deq (l, r)) => l `L.append` (L.rev r)

val toRevList[a]: t a -> list a = fn (Deq (l, r)) => r `L.append` (L.rev l)

fun viewL[a]: t a -> option (a * t a) = fn (Deq (l, r)) =>
   case l of
      | x :: xs => Some (x, Deq (xs, r))
      | [] => (case LU.listSplitLast r of
               | None => None
               | Some (ys, y) => Some (y, fromList (L.rev ys))
               )

fun viewR[a]: t a -> option (t a * a) = fn (Deq (l, r)) =>
    case r of
      | x :: xs => Some (Deq (l, xs), x)
      | [] => (case LU.listSplitLast l of
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
(* subsequences *)

fun take[a] (n: int) (d1:t a): t a =
  case d1 of
   Deq (l, r) =>
     let val len_l = L.length l
     in if n <= L.length l
       then Deq (L.take n l, []) 
       else let Deq (l, r')
            where
              val r' = L.rev r |> L.take (n - len_l)
                               |> L.rev 
            end
     end

fun takeR[a] (n: int) (d1:t a): t a =
  case d1 of
   Deq (l, r) =>
     let val len_r = L.length r
     in if n <= len_r
       then Deq ([], L.take n r)
       else let Deq (l', r)
            where
               val l' = L.rev l |> L.take (n - len_r) |> L.rev 
            end
     end

fun drop[a] (n: int) (d1:t a): t a = 
  case d1 of
   Deq (l, r) =>
     let val len_l = L.length l
     in if n <= len_l
       then Deq (L.drop n l, r)
       else let Deq ([], r')
            where
              val r' = L.rev r |> L.drop (n - len_l) |> L.rev
            end 
     end

fun dropR[a] (n: int) (d1:t a): t a =
  case d1 of
   Deq (l, r) =>
     let val len_r = L.length r
     in if n <= len_r
          then Deq (l, L.drop n r)
          else let Deq (l', [])
               where
                 val l' = L.rev l |> L.drop (n - len_r) |> L.rev
               end 
     end

fun splitAt[a] (n: int) (d1: t a): t a * t a =
    let val (prefix, suffix) = L.splitAt n <| toList d1
    in
        (fromList prefix, fromList suffix)
    end

fun span[a] (prop: a -> bool) (d1: t a): t a * t a =
    let val (prefix, suffix) = LU.listSpan prop <| toList d1
    in (fromList prefix, fromList suffix)
    end

fun takeWhile[a] (prop: a -> bool): t a -> t a = span prop >>> PU.fst
fun dropWhile[a] (prop: a -> bool): t a -> t a = span prop >>> PU.snd

fun spanR[a] (prop: a -> bool) (d1: t a): t a * t a =
    let val (prefix, suffix) = LU.listSpan prop <| toRevList d1
    in (fromList (L.rev prefix), fromList (L.rev suffix))
    end

fun takeWhileR[a] (prop: a -> bool): t a -> t a = spanR prop >>> PU.fst
fun dropWhileR[a] (prop: a -> bool): t a -> t a = spanR prop >>> PU.snd


(* transform *)

val rev[a]: t a -> t a = fn (Deq (l, r)) => Deq (L.rev r, L.rev l)

val mp[a][b]: (a -> b) -> t a -> t b = fn f (Deq (l, r)) =>

     Deq (L.mp f l, L.mp f r)

val mapPartial[a][b]: (a -> option b) -> t a -> t b = fn f (Deq (l, r)) =>

     Deq (L.mapPartial f l, L.mapPartial f r)

(* filter *)

val filter[a]: (a -> bool) -> t a -> t a = fn prop (Deq (l, r)) =>

     Deq (L.filter prop l, L.filter prop r)

val partition[a]: (a -> bool) -> t a -> t a * t a = fn prop (Deq (l, r)) =>
     let
         val (l_pos, l_neg) = LU.listPartition prop l
         val (r_pos, r_neg) = LU.listPartition prop r
     in
         (Deq (l_pos, r_pos), Deq (l_neg, r_neg))
     end

(* foldings *)

val foldl[a][b]: (a -> b -> b) -> b -> t a -> b = fn binop z (Deq (l, r)) =>

    z |> (FU.flip (L.foldl binop) l >>> FU.flip (L.foldr binop) r)

val foldr[a][b]: (a -> b -> b) -> b -> t a -> b = fn binop z (Deq (l, r)) =>

    z |> (FU.flip (L.foldl binop) r >>> FU.flip (L.foldr binop) l)


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

fun propSnocViewR[a] (_:eq a) (x: a) (d1: t a): bool = viewR (snoc d1 x) = Some (d1, x)

fun propFromToList[a] (_:eq a) (li: list a): bool = li = toList ( fromList li)


fun propNthSameElements[a] (_:eq a) (d1: t a): bool =
   let 
      L.mp (nth d1) indexes = L.mp (L.nth (toList d1)) indexes
   where
       val indexes = LU.rangeList 0 (size d1)
   end

fun propTakeDropSplitAt[a] (_:eq a) (d1: t a): bool =
    let
       L.all prop indexes
    where
       val indexes = LU.rangeList 0 (size d1)
       fun prop (n: int): bool = 
            let
               val (fstOfSplit, sndOfSplit) = splitAt n d1
            in
               take n d1 = fstOfSplit
               && drop n d1 = sndOfSplit
            end
    end

fun propTakeRDropRSplitAt[a] (_:eq a) (d1: t a): bool =
    let
       L.all prop indexes
    where
       val len = size d1
       val indexes = LU.rangeList 0 len
       fun prop (n: int): bool =
            let
               val (fstOfSplit, sndOfSplit) = splitAt (len - n) d1
            in
               takeR n d1 = sndOfSplit
               && dropR n d1 = fstOfSplit
            end
    end
   