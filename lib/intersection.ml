type t = { distance : float; shape : Shape.t }

let v shape distance = { shape; distance }
let distance t = t.distance
let shape t = t.shape

let local_sphere_intersects s r =
  let sphere_to_ray = Tuple.subtract (Ray.origin r) (Tuple.point 0. 0. 0.) in
  let a = Tuple.dot (Ray.direction r) (Ray.direction r) in
  let b = Tuple.dot (Ray.direction r) sphere_to_ray *. 2. in
  let c = Tuple.dot sphere_to_ray sphere_to_ray -. 1. in
  let d = (b *. b) -. (4. *. a *. c) in
  match d < 0.0 with
  | true -> []
  | false ->
      [
        v s (((b *. -1.) -. Float.sqrt d) /. (2. *. a));
        v s (((b *. -1.) +. Float.sqrt d) /. (2. *. a));
      ]

let local_plane_intersects s r =
  let direction_y = Tuple.y (Ray.direction r) in
  if Float.abs direction_y < Float.epsilon then []
  else
    let origin_y = Tuple.y (Ray.origin r) in
    let t = (0. -. origin_y) /. direction_y in
    [ v s t ]

let cube_axis_check o d =
  let tmin_enum = -1. -. o and tmax_enum = 1. -. o in
  let tmin, tmax =
    if Float.abs d > Float.epsilon then (tmin_enum /. d, tmax_enum /. d)
    else
      (* this gives them the correct sign *)
      (tmin_enum *. Float.infinity, tmax_enum *. Float.infinity)
  in
  if tmin > tmax then (tmax, tmin) else (tmin, tmax)

let local_cube_intersects s r =
  let o = Ray.origin r and d = Ray.direction r in
  let xmin, xmax = cube_axis_check (Tuple.x o) (Tuple.x d)
  and ymin, ymax = cube_axis_check (Tuple.y o) (Tuple.y d)
  and zmin, zmax = cube_axis_check (Tuple.z o) (Tuple.z d) in
  let tmin = Float.max (Float.max xmin ymin) zmin
  and tmax = Float.min (Float.min xmax ymax) zmax in
  if tmin < tmax then [ v s tmin; v s tmax ] else []

let intersects s r =
  let transform = Shape.inverse_transform s in
  let r = Ray.transform r transform in
  match Shape.geometry s with
  | Shape.Sphere -> local_sphere_intersects s r
  | Shape.Plane -> local_plane_intersects s r
  | Shape.Cube -> local_cube_intersects s r

let sort tl = List.sort (fun a b -> Float.compare a.distance b.distance) tl

let hit tl =
  match tl with
  | [] -> None
  | tl ->
      (* In future we should just ensure this is sorted always *)
      let sorted_lt = sort tl in
      let rec loop tl =
        match tl with
        | [] -> None
        | x :: xs ->
            let d = x.distance in
            if d >= -10. *. Float.epsilon then Some x else loop xs
      in
      loop sorted_lt

let local_sphere_normal_at _s op = Tuple.subtract op (Tuple.point 0. 0. 0.)
let local_plane_normal_at _s _op = Tuple.vector 0. 1. 0.

let local_cube_normal_at _s op =
  let abs_x = Float.abs (Tuple.x op)
  and abs_y = Float.abs (Tuple.y op)
  and abs_z = Float.abs (Tuple.z op) in
  let maxc = Float.max (Float.max abs_x abs_y) abs_z in

  if abs_x = maxc then Tuple.vector (Tuple.x op) 0. 0.
  else if abs_y = maxc then Tuple.vector 0. (Tuple.y op) 0.
  else Tuple.vector 0. 0. (Tuple.z op)

let normal_at s p =
  let pm = Tuple.to_matrix p in
  let itm = Shape.inverse_transform s in
  let object_space_p = Matrix.multiply itm pm in
  let op = Tuple.of_matrix object_space_p in

  let object_normal =
    match Shape.geometry s with
    | Shape.Sphere -> local_sphere_normal_at s op
    | Shape.Plane -> local_plane_normal_at s op
    | Shape.Cube -> local_cube_normal_at s op
  in

  let titm = Shape.transpose_inverse_transform s in
  let world_normal = Matrix.multiply titm (Tuple.to_matrix object_normal) in
  Tuple.normalize
    (Tuple.vector
       (Matrix.cell world_normal (0, 0))
       (Matrix.cell world_normal (1, 0))
       (Matrix.cell world_normal (2, 0)))
