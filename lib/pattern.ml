type style_t = Stripes
type t = { style : style_t; colours : Colour.t * Colour.t }

let v style colours = { style; colours }
let style t = t.style
let colours t = t.colours

let stripes_colour_at t p =
  let x = Tuple.x p in
  let ix = Int.of_float (Float.floor x) in
  let a, b = t.colours in
  if ix mod 2 = 0 then b else a

let colour_at t p = match t.style with Stripes -> stripes_colour_at t p
