type t

val v : float array array -> t
val dimensions : t -> int * int
val cell : t -> int * int -> float
val is_equal : t -> t -> bool
val multiply : t -> t -> t
val identity : int -> t
val transpose : t -> t
