type t

val v : Intersection.t -> Ray.t -> Intersection.t list -> t
val distance : t -> float
val shape : t -> Shape.t
val point : t -> Specialised.t
val over_point : t -> Specialised.t
val under_point : t -> Specialised.t
val eyev : t -> Specialised.t
val normalv : t -> Specialised.t
val reflectv : t -> Specialised.t
val inside : t -> bool
val n_pair : t -> float * float
val schlick : t -> float
