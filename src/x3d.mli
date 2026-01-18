val chunks : int -> 'a list -> ('a list list, string) result

val of_file : ?material:Material.t -> ?transform:Matrix.t -> string -> Shape.t list
