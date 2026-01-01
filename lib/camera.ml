type t = {
  hsize : int;
  vsize : int;
  field_of_view : float;
  transform : Matrix.t;
  inverse_transform : Matrix.t;
  half_width : float;
  half_height : float;
  pixel_size : float;
}

let v ?transform (hsize, vsize) field_of_view =
  let transform, inverse_transform =
    match transform with
    | None -> (Matrix.identity 4, Matrix.identity 4)
    | Some t -> (t, Matrix.inverse t)
  in
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
  {
    hsize;
    vsize;
    field_of_view;
    transform;
    inverse_transform;
    half_width;
    half_height;
    pixel_size;
  }

let dimensions t = (t.hsize, t.vsize)
let field_of_view t = t.field_of_view
let transform t = t.transform
let pixel_size t = t.pixel_size

let ray_for_pixel t (x, y) =
  let xoffset = (Float.of_int x +. 0.5) *. t.pixel_size
  and yoffset = (Float.of_int y +. 0.5) *. t.pixel_size in
  let world_x = t.half_width -. xoffset
  and world_y = t.half_height -. yoffset in

  let point = Tuple.point world_x world_y (-1.) in
  let origin = Tuple.point 0. 0. 0. in

  let pixelm = Matrix.multiply t.inverse_transform (Tuple.to_matrix point) in
  let pixelp = Tuple.of_matrix pixelm in
  let originm = Matrix.multiply t.inverse_transform (Tuple.to_matrix origin) in
  let originp = Tuple.of_matrix originm in
  let direction = Tuple.normalize (Tuple.subtract pixelp originp) in
  Ray.v originp direction
