let translation x y z =
  let data =
    [|
      [| 1.; 0.; 0.; x |];
      [| 0.; 1.; 0.; y |];
      [| 0.; 0.; 1.; z |];
      [| 0.; 0.; 0.; 1. |];
    |]
  in
  Matrix.v data

let scaling x y z =
  let data =
    [|
      [| x; 0.; 0.; 0. |];
      [| 0.; y; 0.; 0. |];
      [| 0.; 0.; z; 0. |];
      [| 0.; 0.; 0.; 1. |];
    |]
  in
  Matrix.v data

let rotate_x r =
  let data =
    [|
      [| 1.; 0.; 0.; 0. |];
      [| 0.; cos r; 0. -. sin r; 0. |];
      [| 0.; sin r; cos r; 0. |];
      [| 0.; 0.; 0.; 1. |];
    |]
  in
  Matrix.v data

let rotate_y r =
  let data =
    [|
      [| cos r; 0.; sin r; 0. |];
      [| 0.; 1.; 0.; 0. |];
      [| 0. -. sin r; 0.; cos r; 0. |];
      [| 0.; 0.; 0.; 1. |];
    |]
  in
  Matrix.v data

let rotate_z r =
  let data =
    [|
      [| cos r; 0. -. sin r; 0.; 0. |];
      [| sin r; cos r; 0.; 0. |];
      [| 0.; 0.; 1.; 0. |];
      [| 0.; 0.; 0.; 1. |];
    |]
  in
  Matrix.v data

let shearing xy xz yx yz zx zy =
  let data =
    [|
      [| 1.; xy; xz; 0. |];
      [| yx; 1.; yz; 0. |];
      [| zx; zy; 1.; 0. |];
      [| 0.; 0.; 0.; 1. |];
    |]
  in
  Matrix.v data

let view_transform from_p to_p up_v =
  if not (Tuple.is_point from_p) then
    raise (Invalid_argument "From must be a point");
  if not (Tuple.is_point to_p) then
    raise (Invalid_argument "To must be a point");
  if not (Tuple.is_vector up_v) then
    raise (Invalid_argument "Up must be a vector");
  let forward = Tuple.normalize (Tuple.subtract to_p from_p) in
  let upn = Tuple.normalize up_v in
  let left = Tuple.cross forward upn in
  let true_up = Tuple.cross left forward in

  let orientation =
    Matrix.v
      [|
        [| Tuple.x left; Tuple.y left; Tuple.z left; 0. |];
        [| Tuple.x true_up; Tuple.y true_up; Tuple.z true_up; 0. |];
        [|
          0. -. Tuple.x forward;
          0. -. Tuple.y forward;
          0. -. Tuple.z forward;
          0.;
        |];
        [| 0.; 0.; 0.; 1. |];
      |]
  in

  let t =
    translation
      (0. -. Tuple.x from_p)
      (0. -. Tuple.y from_p)
      (0. -. Tuple.z from_p)
  in
  Matrix.multiply orientation t
