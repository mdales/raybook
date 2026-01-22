type t

val v : Specialised.t -> Colour.t -> t
val position : t -> Specialised.t
val intensity : t -> Colour.t

val lighting :
  light:t ->
  shape:Shape.t ->
  eye:Specialised.t ->
  normal:Specialised.t ->
  material:Material.t ->
  point:Specialised.t ->
  shadow:bool ->
  unit ->
  Colour.t
