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
