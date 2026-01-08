type geometry_t = Sphere | Plane | Cube

type t = {
  geometry : geometry_t;
  material : Material.t;
  transform : Matrix.t;
  inverse_transform : Matrix.t;
  transpose_inverse_transform : Matrix.t;
}

let v ?material ?transform geometry =
  let material =
    match material with
    | Some m -> m
    | None -> Material.v ~pattern:Pattern.(v (Solid Colour.white)) ()
  in
  let transform, inverse_transform =
    match transform with
    | Some t -> (t, Matrix.inverse t)
    | None -> (Matrix.identity 4, Matrix.identity 4)
  in
  {
    geometry;
    material;
    transform;
    inverse_transform;
    transpose_inverse_transform = Matrix.transpose inverse_transform;
  }

let geometry t = t.geometry
let material t = t.material
let transform t = t.transform
let inverse_transform t = t.inverse_transform
let transpose_inverse_transform t = t.transpose_inverse_transform
