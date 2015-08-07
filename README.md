# urweb-dequeue

Splitted list dequeue

```ocaml
datatype deq a = Deq of (list a * list a)

fun cons[a]: a -> deq a -> deq a = fn x (Deq (l, r)) => Deq (x :: l, r)

fun snoc[a]: a -> deq a -> deq a = fn x (Deq (l, r)) => Deq (l, x :: r)

val toList[a]: deq a -> list a = fn (Deq (l, r)) => l `L.append` (L.rev r)

fun viewL[a]: deq a -> option (a * deq a) = ...
fun viewR[a]: deq a -> option (deq a * a) = ...
```
