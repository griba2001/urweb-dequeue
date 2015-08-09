# urweb-dequeue

Split list dequeue

```ocaml
datatype t a = Deq of (list a * list a)

(* construct *)
fun cons[a]: a -> t a -> t a = fn x (Deq (l, r)) => Deq (x :: l, r)
fun snoc[a]: t a -> a -> t a = fn (Deq (l, r)) x => Deq (l, x :: r)

(* deconstruct *)
fun viewL[a]: t a -> option (a * t a) = ...
fun viewR[a]: t a -> option (t a * a) = ...

(* import/export *)
fun fromList[a]: list a -> t a = fn li =>
     case li of
         [] => Deq ([], [])
         | _ => let val (prefix, suffix) = L.splitAt (L.length li `divide` 2) li
                in Deq (prefix, L.rev suffix)
                end

val toList[a]: t a -> list a = fn (Deq (l, r)) => l `L.append` (L.rev r)
...
```

### test task

tests lib/test/deq_UnitTest UrUnit assertions

```bash
export C_INCLUDE_PATH=/path-to-your-urweb-installation/include
export LIBRARY_PATH=/path-to-your-urweb-installation/lib

cd test/util/c/
gcc -c Random.c
cd ../../..

urweb test1
./test1.exe -p 8082
browser http://localhost:8082
```
Every browser page refresh brings different random input data
