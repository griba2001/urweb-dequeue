# urweb-dequeue

Splitted list dequeue

```ocaml
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

fun viewL[a]: t a -> option (a * t a) = ...
fun viewR[a]: t a -> option (t a * a) = ...
```
