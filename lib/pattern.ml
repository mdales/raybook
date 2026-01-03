type style_t = Solid of Colour.t | Stripes of Colour.t * Colour.t
type t = { style : style_t; transform : Matrix.t; inverse_transform : Matrix.t }

let v ?transform style =
  let transform, inverse_transform =
    match transform with
    | None -> (Matrix.identity 4, Matrix.identity 4)
    | Some t -> (t, Matrix.inverse t)
  in
  { style; transform; inverse_transform }

let transform t = t.transform
let inverse_transform t = t.inverse_transform

let stripes_colour_at (a, b) p =
  let x = Tuple.x p in
  let ix = Int.of_float (Float.floor x) in
  if ix mod 2 = 0 then a else b

let _colour_at t p =
  if not (Tuple.is_point p) then
    raise (Invalid_argument "Expected point not vector");
  match t.style with
  | Solid c -> c
  | Stripes (a, b) -> stripes_colour_at (a, b) p

let colour_at t p =
  if not (Tuple.is_point p) then
    raise (Invalid_argument "Expected point not vector");
  let pm = Tuple.to_matrix p in
  let pattern_space_point = Matrix.multiply t.inverse_transform pm in
  let opp = Tuple.of_matrix pattern_space_point in
  _colour_at t opp
