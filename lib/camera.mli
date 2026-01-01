type t

val v : (int * int) -> float -> t

val dimensions : t -> (int * int)
val field_of_view : t -> float
val transform : t -> Matrix.t
