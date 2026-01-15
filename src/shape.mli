type geometry_t =
  | Cone of { min : float; max : float; capped : bool }
  | Cube
  | Cylinder of { min : float; max : float; capped : bool }
  | Plane
  | Sphere
  | Group of t list

and t

val v : ?material:Material.t -> ?transform:Matrix.t -> geometry_t -> t
val material : t -> Material.t
val transform : t -> Matrix.t
val inverse_transform : t -> Matrix.t
val transpose_inverse_transform : t -> Matrix.t
val geometry : t -> geometry_t
val bounds : t -> Tuple.t * Tuple.t
