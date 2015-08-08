con t :: Type -> Type

val fromList: a ::: Type -> list a -> t a
val toList: a ::: Type -> t a -> list a

val cons:  a ::: Type -> a -> t a -> t a
val snoc:  a ::: Type -> a -> t a -> t a

val viewL: a ::: Type -> t a -> option (a * t a)
val viewR: a ::: Type -> t a -> option (t a * a)

val empty: a ::: Type -> t a
val null: a ::: Type -> t a -> bool
val size: a ::: Type -> t a -> int
val append: a ::: Type -> t a -> t a -> t a

val filter: a ::: Type -> (a -> bool) -> t a -> t a
val mp: a ::: Type -> b ::: Type -> (a -> b) -> t a -> t b
val mapPartial: a ::: Type -> b ::: Type -> (a -> option b) -> t a -> t b

val foldl: a ::: Type -> b ::: Type -> (a -> b -> b) -> b -> t a -> b
val foldr: a ::: Type -> b ::: Type -> (a -> b -> b) -> b -> t a -> b

val foldlPartial: a ::: Type -> b ::: Type -> (a -> b -> option b) -> b -> t a -> b
val foldrPartial: a ::: Type -> b ::: Type -> (a -> b -> option b) -> b -> t a -> b

val foldlAccum: a ::: Type -> b ::: Type -> c ::: Type -> (a -> b -> c * b) -> b -> t a -> t c * b

val sum: a ::: Type -> num a -> t a -> a

val propConsViewL: a ::: Type -> eq a -> a -> t a -> bool
val propSnocViewR: a ::: Type -> eq a -> a -> t a -> bool

