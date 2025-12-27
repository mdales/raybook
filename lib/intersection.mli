type shape_t = Sphere of Sphere.t
type t

val v : shape_t -> float -> t
val distance : t -> float
val shape : t -> shape_t
val intersects : shape_t -> Ray.t -> t list
