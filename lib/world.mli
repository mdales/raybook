type t

val v : Light.t -> Shape.t list -> t
val light : t -> Light.t
val shapes : t -> Shape.t list
val intersect : t -> Ray.t -> Intersection.t list
val shader_hit : t -> Precomputed.t -> Colour.t
