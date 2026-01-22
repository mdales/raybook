let translation x y z =
  let data =
    [|
      [| 1.; 0.; 0.; x |];
      [| 0.; 1.; 0.; y |];
      [| 0.; 0.; 1.; z |];
      [| 0.; 0.; 0.; 1. |];
    |]
  in
  Specialised.of_array data

let scaling x y z =
  let data =
    [|
      [| x; 0.; 0.; 0. |];
      [| 0.; y; 0.; 0. |];
      [| 0.; 0.; z; 0. |];
      [| 0.; 0.; 0.; 1. |];
    |]
  in
  Specialised.of_array data

let rotate_x r =
  let data =
    [|
      [| 1.; 0.; 0.; 0. |];
      [| 0.; cos r; 0. -. sin r; 0. |];
      [| 0.; sin r; cos r; 0. |];
      [| 0.; 0.; 0.; 1. |];
    |]
  in
  Specialised.of_array data

let rotate_y r =
  let data =
    [|
      [| cos r; 0.; sin r; 0. |];
      [| 0.; 1.; 0.; 0. |];
      [| 0. -. sin r; 0.; cos r; 0. |];
      [| 0.; 0.; 0.; 1. |];
    |]
  in
  Specialised.of_array data

let rotate_z r =
  let data =
    [|
      [| cos r; 0. -. sin r; 0.; 0. |];
      [| sin r; cos r; 0.; 0. |];
      [| 0.; 0.; 1.; 0. |];
      [| 0.; 0.; 0.; 1. |];
    |]
  in
  Specialised.of_array data

let shearing xy xz yx yz zx zy =
  let data =
    [|
      [| 1.; xy; xz; 0. |];
      [| yx; 1.; yz; 0. |];
      [| zx; zy; 1.; 0. |];
      [| 0.; 0.; 0.; 1. |];
    |]
  in
  Specialised.of_array data

let view_transform from_p to_p up_v =
  if not (Specialised.is_point from_p) then
    raise (Invalid_argument "From must be a point");
  if not (Specialised.is_point to_p) then
    raise (Invalid_argument "To must be a point");
  if not (Specialised.is_vector up_v) then
    raise (Invalid_argument "Up must be a vector");
  let forward = Specialised.normalize (Specialised.subtract to_p from_p) in
  let upn = Specialised.normalize up_v in
  let left = Specialised.cross forward upn in
  let true_up = Specialised.cross left forward in

  let orientation =
    Specialised.of_array
      [|
        [| Specialised.x left; Specialised.y left; Specialised.z left; 0. |];
        [|
          Specialised.x true_up;
          Specialised.y true_up;
          Specialised.z true_up;
          0.;
        |];
        [|
          0. -. Specialised.x forward;
          0. -. Specialised.y forward;
          0. -. Specialised.z forward;
          0.;
        |];
        [| 0.; 0.; 0.; 1. |];
      |]
  in

  let t =
    translation
      (0. -. Specialised.x from_p)
      (0. -. Specialised.y from_p)
      (0. -. Specialised.z from_p)
  in
  Specialised.multiply orientation t

let combine tl =
  match tl with
  | [] -> Specialised.identity ()
  | hd :: [] -> hd
  | hd :: tl -> List.fold_left (fun a b -> Specialised.multiply b a) hd tl
