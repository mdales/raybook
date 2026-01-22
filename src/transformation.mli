val translation : float -> float -> float -> Specialised.t
val scaling : float -> float -> float -> Specialised.t
val rotate_x : float -> Specialised.t
val rotate_y : float -> Specialised.t
val rotate_z : float -> Specialised.t

val shearing :
  float -> float -> float -> float -> float -> float -> Specialised.t

val view_transform :
  Specialised.t -> Specialised.t -> Specialised.t -> Specialised.t

val combine : Specialised.t list -> Specialised.t
