type t

val v :
  pattern:Pattern.t ->
  ?ambient:float ->
  ?diffuse:float ->
  ?specular:float ->
  ?shininess:float ->
  ?reflectivity:float ->
  unit ->
  t

val ambient : t -> float
val diffuse : t -> float
val specular : t -> float
val shininess : t -> float
val pattern : t -> Pattern.t
val reflectivity : t -> float
