type t

val v : ?material:Material.t -> int -> t
val id : t -> int
val material : t -> Material.t
val set_transform : t -> Matrix.t -> t
val transform : t -> Matrix.t
val inverse_transform : t -> Matrix.t
val transpose_inverse_transform : t -> Matrix.t
