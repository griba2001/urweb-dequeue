(* fun utils *)

fun flip [a][b][c] (f:a -> b -> c) (x: b) (y: a) =  f y x
