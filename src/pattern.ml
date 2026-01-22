type style_t =
  | Solid of Colour.t
  | Stripes of Colour.t * Colour.t
  | Gradient of Colour.t * Colour.t
  | Rings of Colour.t * Colour.t
  | Cubes of Colour.t * Colour.t
  | TestPattern

type t = {
  style : style_t;
  transform : Specialised.t;
  inverse_transform : Specialised.t;
}

let v ?transform style =
  let transform, inverse_transform =
    match transform with
    | None -> (Specialised.identity (), Specialised.identity ())
    | Some t -> (t, Specialised.inverse t)
  in
  { style; transform; inverse_transform }

let style t = t.style
let transform t = t.transform
let inverse_transform t = t.inverse_transform

let stripes_colour_at (a, b) p =
  let x = Specialised.x p in
  let ix = Int.of_float (Float.floor x) in
  if ix mod 2 = 0 then a else b

let rings_colour_at (a, b) p =
  let x = Specialised.x p and z = Specialised.z p in
  let distance = Float.sqrt ((x *. x) +. (z *. z)) in
  let idistance = Int.of_float (Float.floor distance) in
  if idistance mod 2 = 0 then a else b

let channel_gradient fa fb fp =
  let distance = fb -. fa in
  let fraction = fp -. Float.floor fp in
  fa +. (distance *. fraction)

let gradient_colour_at (a, b) p =
  let fp = Specialised.x p in
  let r = channel_gradient (Colour.red a) (Colour.red b) fp
  and g = channel_gradient (Colour.green a) (Colour.green b) fp
  and b = channel_gradient (Colour.blue a) (Colour.blue b) fp in
  Colour.v r g b

let cubes_colour_at (a, b) p =
  let x = Int.of_float (Float.floor (Specialised.x p +. Float.epsilon))
  and y = Int.of_float (Float.floor (Specialised.y p +. Float.epsilon))
  and z = Int.of_float (Float.floor (Specialised.z p +. Float.epsilon)) in
  let dist = x + y + z in
  if dist mod 2 = 0 then a else b

let test_pattern_colour_at p =
  Colour.v (Specialised.x p) (Specialised.y p) (Specialised.z p)

let _colour_at t p =
  if not (Specialised.is_point p) then
    raise (Invalid_argument "Expected point not vector");
  match t.style with
  | Solid c -> c
  | Stripes (a, b) -> stripes_colour_at (a, b) p
  | Gradient (a, b) -> gradient_colour_at (a, b) p
  | Rings (a, b) -> rings_colour_at (a, b) p
  | Cubes (a, b) -> cubes_colour_at (a, b) p
  | TestPattern -> test_pattern_colour_at p

let colour_at t p =
  if not (Specialised.is_point p) then
    raise (Invalid_argument "Expected point not vector");
  let pattern_space_point = Specialised.multiply t.inverse_transform p in
  _colour_at t pattern_space_point
