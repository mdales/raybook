type tick_func = int -> Matrix.t * World.t
(** Function called once a frame during run *)

val run : int * int -> tick_func -> unit
