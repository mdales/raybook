type t = { origin : Specialised.t; direction : Specialised.t }

let v origin direction =
  if not (Specialised.is_point origin) then
    raise (Invalid_argument "Origin must be a point");
  if not (Specialised.is_vector direction) then
    raise (Invalid_argument "Direction must be a vector");
  { origin; direction }

let origin t = t.origin
let direction t = t.direction

let position t x =
  Specialised.add t.origin (Specialised.fmultiply t.direction x)

let transform t m =
  let scaled_om = Specialised.multiply m t.origin
  and scaled_dm = Specialised.multiply m t.direction in
  v scaled_om scaled_dm
