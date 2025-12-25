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
