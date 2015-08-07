(* splitted list dequeue *)

structure L = List
structure O = Option  (* import option eq instance *)

datatype deq a = Deq of (list a * list a)

(*
fun mapPair[a][b][c] (f: b -> c) (p: a * b) = (p.1, f p.2)
*)

fun fromList[a]: list a -> deq a = fn li =>
     case li of
         [] => Deq ([], [])
         | _ => let val (prefix, sufix) = L.splitAt (L.length li `divide` 2) li
                in Deq (prefix, L.rev sufix)
                end   

val toList[a]: deq a -> list a = fn (Deq (l, r)) => l `L.append` (L.rev r)

fun cons[a]: a -> deq a -> deq a = fn x (Deq (l, r)) => Deq (x :: l, r)
fun snoc[a]: a -> deq a -> deq a = fn x (Deq (l, r)) => Deq (l, x :: r)

fun splitLast[a] (acc: list a) (li: list a): option (list a * a) =
     case li of
        [] => None
        | x :: [] => Some (L.rev acc, x)
        | x :: rest => splitLast (x :: acc) rest

fun viewL[a]: deq a -> option (a * deq a) = fn (Deq (l, r)) =>
   case l of
      | x :: xs => Some (x, Deq (xs, r))
      | [] => (case splitLast [] r of
               | None => None
               | Some (ys, y) => Some (y, fromList (L.rev ys))
               )

fun viewR[a]: deq a -> option (deq a * a) = fn (Deq (l, r)) =>
    case r of
      | x :: xs => Some (Deq (l, xs), x)
      | [] => (case splitLast [] l of
               | None => None
               | Some (ys, y) => Some (fromList ys, y)
               )

(* class instances *)

val eq_pair[a][b] (_:eq a) (_:eq b): eq (a * b) =
   let fun eq' (p: a * b) (q: a * b) = p.1 = q.1 && p.2 = q.2
   in mkEq eq'
   end

val eq_deq[a] (_:eq a): eq (deq a) = let fun eq' (d1: deq a) (d2: deq a) = toList d1 = toList d2
                            in mkEq eq'
                            end

val show_deq[a](_:show a): show (deq a) = let fun show' (d1: deq a) = show (toList d1)
                                          in mkShow show'
                                          end       
                                

(* invariants *)

fun propConsViewL[a] (_:eq a) (x: a) (d1: deq a): bool = viewL (cons x d1) = Some (x, d1)

fun propConsViewR[a] (_:eq a) (x: a) (d1: deq a): bool = viewR (snoc x d1) = Some (d1, x)

(* ops *)

val filter[a]: (a -> bool) -> deq a -> deq a = fn prop (Deq (l, r)) => Deq (L.filter prop l, L.filter prop r)

val mp[a][b]: (a -> b) -> deq a -> deq b = fn f (Deq (l, r)) => Deq (L.mp f l, L.mp f r)

