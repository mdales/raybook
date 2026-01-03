type style_t = Solid of Colour.t | Stripes of Colour.t * Colour.t
type t

val v : ?transform:Matrix.t -> style_t -> t
val transform : t -> Matrix.t
val inverse_transform : t -> Matrix.t
val colour_at : t -> Tuple.t -> Colour.t
