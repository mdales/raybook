type t = { origin : Tuple.t; direction : Tuple.t }

let v origin direction =
  if not (Tuple.is_point origin) then
    raise (Invalid_argument "Origin must be a point");
  if not (Tuple.is_vector direction) then
    raise (Invalid_argument "Direction must be a vector");
  { origin; direction }

let origin t = t.origin
let direction t = t.direction
let position t x = Tuple.add t.origin (Tuple.fmultiply t.direction x)

let transform t m =
  let om = Tuple.to_matrix t.origin and dm = Tuple.to_matrix t.direction in
  let scaled_om = Matrix.multiply m om and scaled_dm = Matrix.multiply m dm in
  v (Tuple.of_matrix scaled_om) (Tuple.of_matrix scaled_dm)
