type t

val v : float array array -> t
val dimensions : t -> int * int
val cell : t -> int * int -> float
val is_equal : t -> t -> bool
val multiply : t -> t -> t
val identity : int -> t
val transpose : t -> t
val determinant : t -> float
val submatrix : t -> int * int -> t
val minor : t -> t
val cofactor : t -> t
val invertible : t -> bool
val inverse : t -> t
