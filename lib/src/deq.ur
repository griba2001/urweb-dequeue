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
                                

(* invariants *)

fun propConsViewL[a] (_:eq a) (x: a) (d1: t a): bool = viewL (cons x d1) = Some (x, d1)

fun propSnocViewR[a] (_:eq a) (x: a) (d1: t a): bool = viewR (snoc x d1) = Some (d1, x)

(* ops *)

val filter[a]: (a -> bool) -> t a -> t a = fn prop (Deq (l, r)) =>

     Deq (L.filter prop l, L.filter prop r)

val mp[a][b]: (a -> b) -> t a -> t b = fn f (Deq (l, r)) =>

     Deq (L.mp f l, L.mp f r)

val mapPartial[a][b]: (a -> option b) -> t a -> t b = fn f (Deq (l, r)) =>

     Deq (L.mapPartial f l, L.mapPartial f r)

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
   converts foldings partial function (a -> b -> option b) to (a -> b -> b) *)

fun withPartialBinop[a][b] (f: a -> b -> option b) (x: a) (acc: b): b =
    case f x acc of
      None => acc
      | Some res => res

val foldlPartial[a][b]: (a -> b -> option b) -> b -> t a -> b = fn partialBinop z deq =>

      foldl (withPartialBinop partialBinop) z deq

val foldrPartial[a][b]: (a -> b -> option b) -> b -> t a -> b = fn partialBinop z deq =>

      foldr (withPartialBinop partialBinop) z deq

val foldlAccum[a][b][c]: (a -> b -> c * b) -> b -> t a -> t c * b = fn stateOp ini (Deq (l, r)) =>
     let
         val (list_lRes, st) = L.foldlMap stateOp ini l
         val (list_rRes, st) = L.foldlMap stateOp st (L.rev r)
     in
         (Deq (list_lRes, L.rev list_rRes), st)
     end
