type t

val v : Tuple.t -> Colour.t -> t
val position : t -> Tuple.t
val intensity : t -> Colour.t

val lighting :
  light:t ->
  shape:Shape.t ->
  eye:Tuple.t ->
  normal:Tuple.t ->
  material:Material.t ->
  point:Tuple.t ->
  shadow:bool ->
  unit ->
  Colour.t
