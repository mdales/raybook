type t

val v : Light.t -> Shape.t list -> t
val light : t -> Light.t
val shapes : t -> Shape.t list
val intersect : t -> Ray.t -> Intersection.t list
val shader_hit : t -> Precomputed.t -> Colour.t
val colour_at : t -> Ray.t -> Colour.t
val reflected_colour : t -> Precomputed.t -> Colour.t
val is_shadowed : t -> Tuple.t -> bool
