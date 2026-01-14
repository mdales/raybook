type t

val v : Canvas.t -> t
val render : t -> Camera.t -> World.t -> unit
val is_complete : t -> bool
val wait_for_completion : t -> unit
val teardown : t -> unit
