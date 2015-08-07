con deq :: Type -> Type

val fromList: a ::: Type -> list a -> deq a
val toList: a ::: Type -> deq a -> list a

val cons:  a ::: Type -> a -> deq a -> deq a
val snoc:  a ::: Type -> a -> deq a -> deq a

val viewL: a ::: Type -> deq a -> option (a * deq a)
val viewR: a ::: Type -> deq a -> option (deq a * a)

val filter: a ::: Type -> (a -> bool) -> deq a -> deq a
val mp: a ::: Type -> b ::: Type -> (a -> b) -> deq a -> deq b
val mapPartial: a ::: Type -> b ::: Type -> (a -> option b) -> deq a -> deq b

val foldl: a ::: Type -> b ::: Type -> (a -> b -> b) -> b -> deq a -> b
val foldr: a ::: Type -> b ::: Type -> (a -> b -> b) -> b -> deq a -> b

val propConsViewL: a ::: Type -> eq a -> a -> deq a -> bool 
val propSnocViewR: a ::: Type -> eq a -> a -> deq a -> bool

