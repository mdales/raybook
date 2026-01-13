type geometry_t =
  | Cone of { min : float; max : float; capped : bool }
  | Cube
  | Cylinder of { min : float; max : float; capped : bool }
  | Plane
  | Sphere
  | Group of t list

and t = {
  geometry : geometry_t;
  material : Material.t;
  transform : Matrix.t;
  inverse_transform : Matrix.t;
  transpose_inverse_transform : Matrix.t;
}

let rec v ?material ?transform geometry =
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
  let geometry, transform, inverse_transform =
    match geometry with
    | Group sl ->
        (* Transform into global space *)
        ( Group
            (List.map
               (fun s ->
                 let global_transform = Matrix.multiply transform s.transform in
                 v ~material:s.material ~transform:global_transform s.geometry)
               sl),
          Matrix.identity 4,
          Matrix.identity 4 )
    | s -> (s, transform, inverse_transform)
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
