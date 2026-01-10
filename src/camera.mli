type t

val v : ?transform:Matrix.t -> int * int -> float -> t
val dimensions : t -> int * int
val field_of_view : t -> float
val transform : t -> Matrix.t
val pixel_size : t -> float
val ray_for_pixel : t -> int * int -> Ray.t
