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
  min_bounds : Tuple.t;
  max_bounds : Tuple.t;
}

let corners a b =
  let ax = Tuple.x a
  and ay = Tuple.y a
  and az = Tuple.z a
  and bx = Tuple.x b
  and by = Tuple.y b
  and bz = Tuple.z b in
  [
    a;
    b;
    Tuple.point ax ay bz;
    Tuple.point ax by az;
    Tuple.point bx ay az;
    Tuple.point bx by az;
    Tuple.point bx ay bz;
    Tuple.point ax by bz;
  ]

let group_bounds sl =
  let limits =
    List.map
      (fun s ->
        let minb, maxb = (s.min_bounds, s.max_bounds) in
        let all_corners = corners minb maxb in
        List.map
          (fun p ->
            let pm = Tuple.to_matrix p in
            let nm = Matrix.multiply s.transform pm in
            Tuple.of_matrix nm)
          all_corners)
      sl
    |> List.concat
  in
  let minx, miny, minz, maxx, maxy, maxz =
    List.fold_left
      (fun (minx, miny, minz, maxx, maxy, maxz) p ->
        let x = Tuple.x p and y = Tuple.y p and z = Tuple.z p in
        ( Float.min x minx,
          Float.min y miny,
          Float.min z minz,
          Float.max x maxx,
          Float.max y maxy,
          Float.max z maxz ))
      ( Float.infinity,
        Float.infinity,
        Float.infinity,
        Float.neg_infinity,
        Float.neg_infinity,
        Float.neg_infinity )
      limits
  in
  (Tuple.point minx miny minz, Tuple.point maxx maxy maxz)

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
  (* this has to be second as groups are evaluated in world space *)
  let min_bounds, max_bounds =
    match geometry with
    | Sphere | Cube -> (Tuple.point (-1.) (-1.) (-1.), Tuple.point 1. 1. 1.)
    | Cylinder { min; max; _ } ->
        (Tuple.point (-1.) min (-1.), Tuple.point 1. max 1.)
    | Cone { min; max; _ } ->
        let radius = Float.max (Float.abs min) (Float.abs max) in
        ( Tuple.point (0. -. radius) min (0. -. radius),
          Tuple.point radius max radius )
    | Plane ->
        ( Tuple.point Float.neg_infinity 0. Float.neg_infinity,
          Tuple.point Float.infinity 0. Float.infinity )
    | Group sl -> group_bounds sl
  in
  {
    geometry;
    material;
    transform;
    inverse_transform;
    transpose_inverse_transform = Matrix.transpose inverse_transform;
    min_bounds;
    max_bounds;
  }

let geometry t = t.geometry
let material t = t.material
let transform t = t.transform
let inverse_transform t = t.inverse_transform
let transpose_inverse_transform t = t.transpose_inverse_transform
let bounds t = (t.min_bounds, t.max_bounds)
