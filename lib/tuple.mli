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
