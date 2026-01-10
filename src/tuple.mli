type t

val v : float -> float -> float -> float -> t
val x : t -> float
val y : t -> float
val z : t -> float
val w : t -> float
val is_point : t -> bool
val is_vector : t -> bool
val point : float -> float -> float -> t
val vector : float -> float -> float -> t
val is_equal : t -> t -> bool
val add : t -> t -> t
val subtract : t -> t -> t
val negate : t -> t
val multiply : t -> float -> t
val divide : t -> float -> t
val magnitude : t -> float
val normalize : t -> t
val dot : t -> t -> float
val cross : t -> t -> t
val to_matrix : t -> Matrix.t
val of_matrix : Matrix.t -> t
val reflect : t -> t -> t
