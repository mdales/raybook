type t

val v : Light.t -> Shape.t list -> t
val light : t -> Light.t
val shapes : t -> Shape.t list
val intersect : t -> Ray.t -> Intersection.t list
val shader_hit : ?count:int -> t -> Precomputed.t -> Colour.t
val colour_at : ?count:int -> t -> Ray.t -> Colour.t
val reflected_colour : ?count:int -> t -> Precomputed.t -> Colour.t
val refracted_colour : ?count:int -> t -> Precomputed.t -> Colour.t
val is_shadowed : t -> Specialised.t -> bool
