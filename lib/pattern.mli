type style_t = Stripes
type t

val v : style_t -> Colour.t * Colour.t -> t
val style : t -> style_t
val colours : t -> Colour.t * Colour.t
val colour_at : t -> Tuple.t -> Colour.t
