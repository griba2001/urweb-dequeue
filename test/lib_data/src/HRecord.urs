(* HRecord *)


val overwrite : a ::: {Type} -> b ::: {Type} -> b' ::: {Type} -> [a ~ b] => [a ~ b'] => $(a ++ b) -> $b' -> $(a ++ b')
