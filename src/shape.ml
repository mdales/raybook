type geometry_t =
  | Cone of { min : float; max : float; capped : bool }
  | Cube
  | Cylinder of { min : float; max : float; capped : bool }
  | Plane
  | Sphere
  | Triangle of (Specialised.t * Specialised.t * Specialised.t)
  | Group of t list

and t = {
  geometry : geometry_t;
  material : Material.t;
  transform : Specialised.t;
  inverse_transform : Specialised.t;
  transpose_inverse_transform : Specialised.t;
  min_bounds : Specialised.t;
  max_bounds : Specialised.t;
  (* valid for triangle only *)
  edges : (Specialised.t * Specialised.t) option;
  normal : Specialised.t option;
}

let corners a b =
  let ax = Specialised.x a
  and ay = Specialised.y a
  and az = Specialised.z a
  and bx = Specialised.x b
  and by = Specialised.y b
  and bz = Specialised.z b in
  [
    a;
    b;
    Specialised.point ax ay bz;
    Specialised.point ax by az;
    Specialised.point bx ay az;
    Specialised.point bx by az;
    Specialised.point bx ay bz;
    Specialised.point ax by bz;
  ]

let group_bounds sl =
  let limits =
    List.map
      (fun s ->
        let minb, maxb = (s.min_bounds, s.max_bounds) in
        let all_corners = corners minb maxb in
        List.map (fun p -> Specialised.multiply s.transform p) all_corners)
      sl
    |> List.concat
  in
  let minx, miny, minz, maxx, maxy, maxz =
    List.fold_left
      (fun (minx, miny, minz, maxx, maxy, maxz) p ->
        let x = Specialised.x p
        and y = Specialised.y p
        and z = Specialised.z p in
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
  (Specialised.point minx miny minz, Specialised.point maxx maxy maxz)

let rec v ?material ?transform geometry =
  let material =
    match material with
    | Some m -> m
    | None -> Material.v ~pattern:Pattern.(v (Solid Colour.white)) ()
  in
  let transform, inverse_transform =
    match transform with
    | Some t -> (t, Specialised.inverse t)
    | None -> (Specialised.identity (), Specialised.identity ())
  in
  let edges, normal =
    match geometry with
    | Triangle (a, b, c) ->
        let e1 = Specialised.subtract b a and e2 = Specialised.subtract c a in
        let normal = Specialised.normalize (Specialised.cross e2 e1) in
        (Some (e1, e2), Some normal)
    | _ -> (None, None)
  in
  let geometry, transform, inverse_transform =
    match geometry with
    | Group sl ->
        (* Transform into global space *)
        ( Group
            (List.map
               (fun s ->
                 let global_transform =
                   Specialised.multiply transform s.transform
                 in
                 v ~material:s.material ~transform:global_transform s.geometry)
               sl),
          Specialised.identity (),
          Specialised.identity () )
    | s -> (s, transform, inverse_transform)
  in
  (* this has to be second as groups are evaluated in world space *)
  let min_bounds, max_bounds =
    match geometry with
    | Sphere | Cube ->
        (Specialised.point (-1.) (-1.) (-1.), Specialised.point 1. 1. 1.)
    | Cylinder { min; max; _ } ->
        (Specialised.point (-1.) min (-1.), Specialised.point 1. max 1.)
    | Cone { min; max; _ } ->
        let radius = Float.max (Float.abs min) (Float.abs max) in
        ( Specialised.point (0. -. radius) min (0. -. radius),
          Specialised.point radius max radius )
    | Plane ->
        ( Specialised.point Float.neg_infinity 0. Float.neg_infinity,
          Specialised.point Float.infinity 0. Float.infinity )
    | Triangle (a, b, c) ->
        let ax = Specialised.x a
        and ay = Specialised.y a
        and az = Specialised.z a
        and bx = Specialised.x b
        and by = Specialised.y b
        and bz = Specialised.z b
        and cx = Specialised.x c
        and cy = Specialised.y c
        and cz = Specialised.z c in
        ( Specialised.point
            (Float.min (Float.min ax bx) cx)
            (Float.min (Float.min ay by) cy)
            (Float.min (Float.min az bz) cz),
          Specialised.point
            (Float.max (Float.max ax bx) cx)
            (Float.max (Float.max ay by) cy)
            (Float.max (Float.max az bz) cz) )
    | Group sl -> group_bounds sl
  in
  {
    geometry;
    material;
    transform;
    inverse_transform;
    transpose_inverse_transform = Specialised.transpose inverse_transform;
    min_bounds;
    max_bounds;
    edges;
    normal;
  }

let geometry t = t.geometry
let material t = t.material
let transform t = t.transform
let inverse_transform t = t.inverse_transform
let transpose_inverse_transform t = t.transpose_inverse_transform
let bounds t = (t.min_bounds, t.max_bounds)
let edges t = t.edges
let normal t = t.normal
