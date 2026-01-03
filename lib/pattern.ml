type t = Solid of Colour.t | Stripes of Colour.t * Colour.t

let stripes_colour_at (a, b) p =
  let x = Tuple.x p in
  let ix = Int.of_float (Float.floor x) in
  if ix mod 2 = 0 then a else b

let colour_at t p =
  match t with Solid c -> c | Stripes (a, b) -> stripes_colour_at (a, b) p
