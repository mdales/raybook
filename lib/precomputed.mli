type t

val v : Intersection.t -> Ray.t -> t
val distance : t -> float
val shape : t -> Shape.t
val point : t -> Tuple.t
val over_point : t -> Tuple.t
val eyev : t -> Tuple.t
val normalv : t -> Tuple.t
val reflectv : t -> Tuple.t
val inside : t -> bool
