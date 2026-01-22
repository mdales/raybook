type t

val v : Specialised.t -> Specialised.t -> t
val origin : t -> Specialised.t
val direction : t -> Specialised.t
val position : t -> float -> Specialised.t
val transform : t -> Specialised.t -> t
