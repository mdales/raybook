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

let intersects s r =
  let transform = Shape.inverse_transform s in
  let r = Ray.transform r transform in
  match Shape.geometry s with
  | Shape.Sphere -> local_sphere_intersects s r
  | Shape.Plane -> local_plane_intersects s r

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
            if d >= 0. then Some x else loop xs
      in
      loop sorted_lt

let local_sphere_normal_at _s op = Tuple.subtract op (Tuple.point 0. 0. 0.)
let local_plane_normal_at _s _op = Tuple.vector 0. 1. 0.

let normal_at s p =
  let pm = Tuple.to_matrix p in
  let itm = Shape.inverse_transform s in
  let object_space_p = Matrix.multiply itm pm in
  let op = Tuple.of_matrix object_space_p in

  let object_normal =
    match Shape.geometry s with
    | Shape.Sphere -> local_sphere_normal_at s op
    | Shape.Plane -> local_plane_normal_at s op
  in

  let titm = Shape.transpose_inverse_transform s in
  let world_normal = Matrix.multiply titm (Tuple.to_matrix object_normal) in
  Tuple.normalize
    (Tuple.vector
       (Matrix.cell world_normal (0, 0))
       (Matrix.cell world_normal (1, 0))
       (Matrix.cell world_normal (2, 0)))
