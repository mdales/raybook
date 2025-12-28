type t = { position : Tuple.t; intensity : Colour.t }

let v position intensity =
  if not (Tuple.is_point position) then
    raise (Invalid_argument "Position must be a point");
  { position; intensity }

let position t = t.position
let intensity t = t.intensity
