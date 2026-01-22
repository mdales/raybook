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
let fp_equal a b = abs_float (a -. b) < epsilon_float *. 10.

let is_equal t o =
  fp_equal t.x o.x && fp_equal t.y o.y && fp_equal t.z o.z && t.w == o.w

let add t o =
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

let subtract t o =
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
  | Vector -> subtract (vector 0. 0. 0.) t
  | Point -> raise (Invalid_argument "Cannot negate point")

let fmultiply t n =
  match t.w with
  | Vector -> { x = t.x *. n; y = t.y *. n; z = t.z *. n; w = t.w }
  | Point -> raise (Invalid_argument "Cannot multiply point")

let fdivide t n =
  match t.w with
  | Vector -> { x = t.x /. n; y = t.y /. n; z = t.z /. n; w = t.w }
  | Point -> raise (Invalid_argument "Cannot divide point")

let magnitude t =
  match t.w with
  | Vector -> Float.sqrt ((t.x *. t.x) +. (t.y *. t.y) +. (t.z *. t.z))
  | Point -> raise (Invalid_argument "Cannot take magnitude of point")

let normalize t =
  match t.w with
  | Vector ->
      let m = magnitude t in
      if m = 0. then t
      else { x = t.x /. m; y = t.y /. m; z = t.z /. m; w = t.w }
  | Point -> raise (Invalid_argument "Cannot normalize point")

let dot t o =
  match (t, o) with
  | ( { x = x0; y = y0; z = z0; w = Vector },
      { x = x1; y = y1; z = z1; w = Vector } ) ->
      (x0 *. x1) +. (y0 *. y1) +. (z0 *. z1)
  | _ -> raise (Invalid_argument "Cannot use points in dot product")

let cross t o =
  match (t, o) with
  | ( { x = x0; y = y0; z = z0; w = Vector },
      { x = x1; y = y1; z = z1; w = Vector } ) ->
      {
        x = (y0 *. z1) -. (z0 *. y1);
        y = (z0 *. x1) -. (x0 *. z1);
        z = (x0 *. y1) -. (y0 *. x1);
        w = Vector;
      }
  | _ -> raise (Invalid_argument "Cannot use points in cross product")

let to_matrix t =
  let data =
    [|
      [| t.x |];
      [| t.y |];
      [| t.z |];
      [| (match t.w with Vector -> 0. | Point -> 1.) |];
    |]
  in
  Matrix.v data

let of_matrix m =
  let dims = Matrix.dimensions m in
  match dims with
  | 4, 1 ->
      let x = Matrix.cell m (0, 0)
      and y = Matrix.cell m (1, 0)
      and z = Matrix.cell m (2, 0)
      and raw_w = Matrix.cell m (3, 0) in
      let w =
        if fp_equal raw_w 0. then Vector
        else if fp_equal raw_w 1. then Point
        else raise (Invalid_argument "W should be 0. or 1. for Vector or Point")
      in
      { x; y; z; w }
  | _ -> raise (Invalid_argument "Matrix expected to have 4x1 dimensions")

let reflect v n =
  match (v.w, n.w) with
  | Vector, Vector -> subtract v (fmultiply n (2. *. dot v n))
  | _ -> raise (Invalid_argument "Cannot use points in dot product")
