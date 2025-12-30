type t

val v : Light.t -> Shape.t list -> t
val light : t -> Light.t
val shapes : t -> Shape.t list
