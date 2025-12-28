type t

val v :
  colour:Colour.t ->
  ?ambient:float ->
  ?diffuse:float ->
  ?specular:float ->
  ?shininess:float ->
  unit ->
  t

val ambient : t -> float
val diffuse : t -> float
val specular : t -> float
val shininess : t -> float
val colour : t -> Colour.t
