type t = {
  array : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;
  width : int;
  height : int;
}

let v dimensions =
  let width, height = dimensions in
  if width <= 0 then raise (Invalid_argument "Invalid width");
  if height <= 0 then raise (Invalid_argument "Invalid height");
  let array =
    Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (width * height)
  in
  { array; width; height }

let dimensions t = (t.width, t.height)

let write_pixel t (x, y) c =
  if x < 0 then raise (Invalid_argument "Invalid x");
  if y < 0 then raise (Invalid_argument "Invalid y");
  let w, h = dimensions t in
  if x >= w then raise (Invalid_argument "Invalid x");
  if y >= h then raise (Invalid_argument "Invalud y");
  let rgb = Colour.to_rgb c in
  t.array.{x + (y * t.width)} <- rgb

let read_pixel t (x, y) =
  if x < 0 then raise (Invalid_argument "Invalid x");
  if y < 0 then raise (Invalid_argument "Invalid y");
  let w, h = dimensions t in
  if x >= w then raise (Invalid_argument "Invalid x");
  if y >= h then raise (Invalid_argument "Invalud y");
  let rgb = t.array.{x + (y * t.width)} in
  Colour.of_rgb rgb
