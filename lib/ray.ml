type t = { origin : Tuple.t; direction : Tuple.t }

let v origin direction =
  if not (Tuple.is_point origin) then
    raise (Invalid_argument "Origin must be a point");
  if not (Tuple.is_vector direction) then
    raise (Invalid_argument "Direction must be a vector");
  { origin; direction }

let origin t = t.origin
let direction t = t.direction
