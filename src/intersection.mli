type t

val v : Shape.t -> float -> t
val distance : t -> float
val shape : t -> Shape.t
val intersects : Shape.t -> Ray.t -> t list
val normal_at : Shape.t -> Tuple.t -> Tuple.t
val hit : t list -> t option
val sort : t list -> t list
