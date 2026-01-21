type t

val of_matrix : Matrix.t -> t
val of_tuple : Tuple.t -> t
val of_array : float array array -> t
val of_point : float -> float -> float -> t
val of_vector : float -> float -> float -> t
val cell : t -> int * int -> float
val is_equal : t -> t -> bool
val multiply : t -> t -> t
val identity : unit -> t
val transpose : t -> t
val submatrix : t -> int * int -> t
val determinant : t -> float
val minor : t -> t
val cofactor : t -> t
val invertible : t -> bool
val inverse : t -> t

(* val x : t -> float
val y : t -> float
val z : t -> float
val is_point : t -> bool
val is_vector : t -> bool
val add : t -> t -> t
val subtract : t -> t -> t
val negate : t -> t
val fmultiply : t -> float -> t
val fdivide : t -> float -> t
val magnitude : t -> float
val normalize : t -> t
val dot : t -> t -> float
val cross : t -> t -> t
val reflect : t -> t -> t *)
