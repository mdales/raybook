type style_t =
  | Solid of Colour.t
  | Stripes of Colour.t * Colour.t
  | Gradient of Colour.t * Colour.t
  | Rings of Colour.t * Colour.t
  | Cubes of Colour.t * Colour.t
  | TestPattern

type t

val v : ?transform:Specialised.t -> style_t -> t
val transform : t -> Specialised.t
val inverse_transform : t -> Specialised.t
val colour_at : t -> Specialised.t -> Colour.t
val style : t -> style_t
