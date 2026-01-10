type geometry_t = Cube | Cylinder of float * float | Plane | Sphere
type t

val v : ?material:Material.t -> ?transform:Matrix.t -> geometry_t -> t
val material : t -> Material.t
val transform : t -> Matrix.t
val inverse_transform : t -> Matrix.t
val transpose_inverse_transform : t -> Matrix.t
val geometry : t -> geometry_t
