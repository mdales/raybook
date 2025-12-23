type t

val v : float -> float -> float -> t
val red : t -> float
val green : t -> float
val blue : t -> float
val add : t -> t -> t
val subtract : t -> t -> t
val fmultiply : t -> float -> t
val multiply : t -> t -> t
val is_equal : t -> t -> bool
val rgb : t -> int32
