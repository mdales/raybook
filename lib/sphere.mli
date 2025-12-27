type t

val v : int -> t
val id : t -> int
val set_transform : t -> Matrix.t -> t
val transform : t -> Matrix.t
