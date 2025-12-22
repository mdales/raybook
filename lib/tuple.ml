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
let fp_equal a b = abs_float (a -. b) < epsilon_float

let is_equal t o =
  fp_equal t.x o.x && fp_equal t.y o.y && fp_equal t.z o.z && t.w == o.w

let sum t o =
  match (t, o) with
  | ( { x = x0; y = y0; z = z0; w = Vector },
      { x = x1; y = y1; z = z1; w = Vector } ) ->
      vector (x0 +. x1) (y0 +. y1) (z0 +. z1)
  | ( { x = x0; y = y0; z = z0; w = Point },
      { x = x1; y = y1; z = z1; w = Vector } ) ->
      point (x0 +. x1) (y0 +. y1) (z0 +. z1)
  | ( { x = x0; y = y0; z = z0; w = Vector },
      { x = x1; y = y1; z = z1; w = Point } ) ->
      point (x0 +. x1) (y0 +. y1) (z0 +. z1)
  | _ -> raise (Invalid_argument "Cannot add two points")

let sub t o =
  match (t, o) with
  | { x = x0; y = y0; z = z0; w = Point }, { x = x1; y = y1; z = z1; w = Point }
    ->
      vector (x0 -. x1) (y0 -. y1) (z0 -. z1)
  | ( { x = x0; y = y0; z = z0; w = Point },
      { x = x1; y = y1; z = z1; w = Vector } ) ->
      point (x0 -. x1) (y0 -. y1) (z0 -. z1)
  | ( { x = x0; y = y0; z = z0; w = Vector },
      { x = x1; y = y1; z = z1; w = Vector } ) ->
      vector (x0 -. x1) (y0 -. y1) (z0 -. z1)
  | _ -> raise (Invalid_argument "Cannot subtract point from vector")

let negate t =
  match t.w with
  | Vector -> sub (vector 0. 0. 0.) t
  | _ -> raise (Invalid_argument "Cannot negate point")

let multiply t n =
  match t.w with
  | Vector -> { x = t.x *. n; y = t.y *. n; z = t.z *. n; w = t.w }
  | _ -> raise (Invalid_argument "Cannot multiply point")

let divide t n =
  match t.w with
  | Vector -> { x = t.x /. n; y = t.y /. n; z = t.z /. n; w = t.w }
  | _ -> raise (Invalid_argument "Cannot divide point")

let magnitude t =
  match t.w with
  | Vector -> Float.sqrt ((t.x *. t.x) +. (t.y *. t.y) +. (t.z *. t.z))
  | _ -> raise (Invalid_argument "Cannot take magnitude of point")
