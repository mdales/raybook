type t = Solid of Colour.t | Stripes of Colour.t * Colour.t

val colour_at : t -> Tuple.t -> Colour.t
