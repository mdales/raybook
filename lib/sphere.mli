type t

val v : int -> t
val id : t -> int
val intersects : t -> Ray.t -> (float * float) option
