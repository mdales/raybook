type t

val v : int * int -> t
val dimensions : t -> int * int
val write_pixel : t -> int * int -> Colour.t -> unit
val read_pixel : t -> int * int -> Colour.t
