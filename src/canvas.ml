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
let raw t = t.array

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

let save_png t filename =
  let img = Rgb24.create t.width t.height in

  for y = 0 to t.height - 1 do
    for x = 0 to t.width - 1 do
      let rgba = t.array.{x + (y * t.width)} in
      let r = Int32.to_int (Int32.shift_right_logical rgba 16) land 0xFF in
      let g = Int32.to_int (Int32.shift_right_logical rgba 8) land 0xFF in
      let b = Int32.to_int (Int32.shift_right_logical rgba 0) land 0xFF in

      Rgb24.set img x y { Color.r; g; b }
    done
  done;

  Png.save filename [] (Images.Rgb24 img)
