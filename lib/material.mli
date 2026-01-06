type t

val v :
  pattern:Pattern.t ->
  ?ambient:float ->
  ?diffuse:float ->
  ?reflectivity:float ->
  ?refractive_index:float ->
  ?specular:float ->
  ?shininess:float ->
  ?transparency:float ->
  unit ->
  t

val ambient : t -> float
val diffuse : t -> float
val pattern : t -> Pattern.t
val reflectivity : t -> float
val refractive_index : t -> float
val shininess : t -> float
val specular : t -> float
val transparency : t -> float
