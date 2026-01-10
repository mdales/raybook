type t

val v : Intersection.t -> Ray.t -> Intersection.t list -> t
val distance : t -> float
val shape : t -> Shape.t
val point : t -> Tuple.t
val over_point : t -> Tuple.t
val under_point : t -> Tuple.t
val eyev : t -> Tuple.t
val normalv : t -> Tuple.t
val reflectv : t -> Tuple.t
val inside : t -> bool
val n_pair : t -> float * float
val schlick : t -> float
