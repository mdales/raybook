type t = {
  hsize : int;
  vsize : int;
  field_of_view : float;
  half_width : float;
  half_height : float;
  pixel_size : float;
}

let v (hsize, vsize) field_of_view =
  if hsize <= 0 then
    raise (Invalid_argument "Horizontal size must be greater than 0");
  if vsize <= 0 then
    raise (Invalid_argument "Vertical size must be greater than 0");
  if field_of_view <= 0. then
    raise (Invalid_argument "Field of view must be greater than 0.");
  let half_view = tan (field_of_view /. 2.) in
  let aspect = Float.of_int hsize /. Float.of_int vsize in
  let half_width, half_height =
    if aspect >= 1. then (half_view, half_view /. aspect)
    else (half_view *. aspect, half_view)
  in
  let pixel_size = half_width *. 2. /. Float.of_int hsize in
  { hsize; vsize; field_of_view; half_width; half_height; pixel_size }

let dimensions t = (t.hsize, t.vsize)
let virtual_dimensions t = (t.half_width, t.half_height)
let field_of_view t = t.field_of_view
let transform _t = Matrix.identity 4
let pixel_size t = t.pixel_size
