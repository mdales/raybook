type geometry_t =
  | Cone of { min : float; max : float; capped : bool }
  | Cube
  | Cylinder of { min : float; max : float; capped : bool }
  | Plane
  | Sphere
  | Triangle of (Specialised.t * Specialised.t * Specialised.t)
  | Group of t list

and t

val v : ?material:Material.t -> ?transform:Specialised.t -> geometry_t -> t
val material : t -> Material.t
val transform : t -> Specialised.t
val inverse_transform : t -> Specialised.t
val transpose_inverse_transform : t -> Specialised.t
val geometry : t -> geometry_t
val bounds : t -> Specialised.t * Specialised.t
val edges : t -> (Specialised.t * Specialised.t) option
val normal : t -> Specialised.t option
