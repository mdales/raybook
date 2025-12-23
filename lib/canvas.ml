type t = Colour.t array array

let v dimensions =
  let width, height = dimensions in
  if width <= 0 then raise (Invalid_argument "Invalid width");
  if height <= 0 then raise (Invalid_argument "Invalid height");
  Array.init height (fun _ -> Array.init width (fun _ -> Colour.v 0. 0. 0.))

let dimensions t =
  let height = Array.length t in
  match height with 0 -> (0, 0) | _ -> (Array.length t.(0), height)

let write_pixel t (x, y) c =
  if x < 0 then raise (Invalid_argument "Invalid x");
  if y < 0 then raise (Invalid_argument "Invalid y");
  let w, h = dimensions t in
  if x >= w then raise (Invalid_argument "Invalid x");
  if y >= h then raise (Invalid_argument "Invalud y");
  t.(y).(x) <- c

let read_pixel t (x, y) =
  if x < 0 then raise (Invalid_argument "Invalid x");
  if y < 0 then raise (Invalid_argument "Invalid y");
  let w, h = dimensions t in
  if x >= w then raise (Invalid_argument "Invalid x");
  if y >= h then raise (Invalid_argument "Invalud y");
  t.(y).(x)
