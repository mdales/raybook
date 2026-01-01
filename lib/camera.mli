type t

val v : int * int -> float -> t
val dimensions : t -> int * int
val field_of_view : t -> float
val transform : t -> Matrix.t
val pixel_size : t -> float
val virtual_dimensions : t -> float * float
