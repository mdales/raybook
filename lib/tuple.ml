type style = Point | Vector
type t = { x : float; y : float; z : float; w : style }

let v x y z w =
  let w =
    match w with
    | 1.0 -> Point
    | 0.0 -> Vector
    | _ -> raise (Invalid_argument "W must be 1.0 or 0.0")
  in
  { x; y; z; w }

let point x y z = v x y z 1.0
let vector x y z = v x y z 0.0
let x t = t.x
let y t = t.y
let z t = t.z
let w t = match t.w with Point -> 1.0 | Vector -> 0.0
let is_point t = t.w == Point
let is_vector t = t.w == Vector

let fp_equal a b =
  abs_float(a -. b) < epsilon_float

let is_equal t o =
  (fp_equal t.x o.x) &&
  (fp_equal t.y o.y) &&
  (fp_equal t.z o.z) &&
  (t.w == o.w)
