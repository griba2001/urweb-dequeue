con t :: Type -> Type

(* construct *)
val cons:  a ::: Type -> a -> t a -> t a
val snoc:  a ::: Type -> t a -> a -> t a

(* deconstruct *)
val viewL: a ::: Type -> t a -> option (a * t a)
val viewR: a ::: Type -> t a -> option (t a * a)

(* import/export *)
val fromList: a ::: Type -> list a -> t a
val toList: a ::: Type -> t a -> list a

(* monoid ops *)
val empty: a ::: Type -> t a
val append: a ::: Type -> t a -> t a -> t a

(* query *)
val null: a ::: Type -> t a -> bool
val size: a ::: Type -> t a -> int
val member: a ::: Type -> eq a -> a -> t a -> bool
val find: a ::: Type -> (a -> bool) -> t a -> option a
val nth: a ::: Type -> t a -> int -> option a

(* subsequences *)

val take: a ::: Type -> int -> t a -> t a
val drop: a ::: Type -> int -> t a -> t a

(* map/filter/fold ops *)

val mp: a ::: Type -> b ::: Type -> (a -> b) -> t a -> t b
val mapPartial: a ::: Type -> b ::: Type -> (a -> option b) -> t a -> t b

val filter: a ::: Type -> (a -> bool) -> t a -> t a

val foldl: a ::: Type -> b ::: Type -> (a -> b -> b) -> b -> t a -> b
val foldr: a ::: Type -> b ::: Type -> (a -> b -> b) -> b -> t a -> b

val foldlPartial: a ::: Type -> b ::: Type -> (a -> b -> option b) -> b -> t a -> b
val foldrPartial: a ::: Type -> b ::: Type -> (a -> b -> option b) -> b -> t a -> b

val filterFoldl: a ::: Type -> b ::: Type -> (a -> bool) -> (a -> b -> b) -> b -> t a -> b
val filterFoldr: a ::: Type -> b ::: Type -> (a -> bool) -> (a -> b -> b) -> b -> t a -> b

val foldlAccum: a ::: Type -> b ::: Type -> c ::: Type -> (a -> b -> c * b) -> b -> t a -> t c * b

(* specific folds *)

val sum: a ::: Type -> num a -> t a -> a
val float_prod: t float -> float

val all: a ::: Type -> (a -> bool) -> t a -> bool
val any: a ::: Type -> (a -> bool) -> t a -> bool

(* invariants *)
val propConsViewL: a ::: Type -> eq a -> a -> t a -> bool
val propSnocViewR: a ::: Type -> eq a -> a -> t a -> bool

val propFromToList: a ::: Type -> eq a -> list a -> bool
val propNthSameElements: a ::: Type -> eq a -> t a -> bool

