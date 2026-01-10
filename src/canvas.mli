type t

val v : int * int -> t
val dimensions : t -> int * int
val write_pixel : t -> int * int -> Colour.t -> unit
val read_pixel : t -> int * int -> Colour.t
val raw : t -> (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
