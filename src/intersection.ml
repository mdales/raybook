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

let check_cap r t radius =
  let o = Ray.origin r in
  let d = Ray.direction r in
  let x = Tuple.x o +. (t *. Tuple.x d) and z = Tuple.z o +. (t *. Tuple.z d) in
  (x *. x) +. (z *. z) <= radius *. radius

let intersect_caps min max capped s r il is_cone =
  match capped with
  | false -> il
  | true -> (
      let dy = Tuple.y (Ray.direction r) in
      match Float.abs dy < Float.epsilon with
      | true -> il
      | false -> (
          let oy = Tuple.y (Ray.origin r) in
          let t0 = (min -. oy) /. dy and t1 = (max -. oy) /. dy in
          let c0 = check_cap r t0 (if is_cone then Float.abs min else 1.)
          and c1 = check_cap r t1 (if is_cone then Float.abs max else 1.) in
          match (c0, c1) with
          | false, false -> il
          | true, false -> v s t0 :: il
          | false, true -> v s t1 :: il
          | true, true -> [ v s t0; v s t1 ] @ il))

let local_cylinder_intersects min max capped s r =
  let d = Ray.direction r in
  let dx = Tuple.x d and dz = Tuple.z d in
  let a = (dx *. dx) +. (dz *. dz) in
  let il =
    match Float.abs a < Float.epsilon with
    | true -> []
    | false -> (
        let o = Ray.origin r in
        let ox = Tuple.x o and oz = Tuple.z o in
        let b = (2. *. ox *. dx) +. (2. *. oz *. dz)
        and c = (ox *. ox) +. (oz *. oz) -. 1. in
        let disc = (b *. b) -. (4. *. a *. c) in
        match disc < 0. with
        | true -> []
        | false -> (
            let t0 = ((b *. -1.) -. Float.sqrt disc) /. (2. *. a)
            and t1 = ((b *. -1.) +. Float.sqrt disc) /. (2. *. a) in
            let t0, t1 = if t0 > t1 then (t1, t0) else (t0, t1) in

            let y0 = Tuple.y o +. (t0 *. Tuple.y d)
            and y1 = Tuple.y o +. (t1 *. Tuple.y d) in

            let y0in = y0 > min && y0 < max and y1in = y1 > min && y1 < max in
            match (y0in, y1in) with
            | false, false -> []
            | true, false -> [ v s t0 ]
            | false, true -> [ v s t1 ]
            | true, true -> [ v s t0; v s t1 ]))
  in
  intersect_caps min max capped s r il false

let local_cone_intersects min max capped s r =
  let d = Ray.direction r in
  let dx = Tuple.x d and dy = Tuple.y d and dz = Tuple.z d in
  let o = Ray.origin r in
  let ox = Tuple.x o and oy = Tuple.y o and oz = Tuple.z o in
  let a = (dx *. dx) -. (dy *. dy) +. (dz *. dz)
  and b = (2. *. ox *. dx) -. (2. *. oy *. dy) +. (2. *. oz *. dz)
  and c = (ox *. ox) -. (oy *. oy) +. (oz *. oz) in

  let il =
    match Float.abs a < Float.epsilon *. 100. with
    | true -> (
        match Float.abs b < Float.epsilon *. 100. with
        | true -> []
        | false ->
            let t = (0. -. c) /. (2. *. b) in
            [ v s t ])
    | false -> (
        let disc = (b *. b) -. (4. *. a *. c) in
        let t0 = ((b *. -1.) -. Float.sqrt disc) /. (2. *. a)
        and t1 = ((b *. -1.) +. Float.sqrt disc) /. (2. *. a) in
        let t0, t1 = if t0 > t1 then (t1, t0) else (t0, t1) in

        let y0 = Tuple.y o +. (t0 *. Tuple.y d)
        and y1 = Tuple.y o +. (t1 *. Tuple.y d) in

        let y0in = y0 > min && y0 < max and y1in = y1 > min && y1 < max in
        match (y0in, y1in) with
        | false, false -> []
        | true, false -> [ v s t0 ]
        | false, true -> [ v s t1 ]
        | true, true -> [ v s t0; v s t1 ])
  in

  intersect_caps min max capped s r il true

let rec local_group_intersects sl _s r =
  let il = List.map (fun is -> intersects is r) sl |> List.concat in
  List.sort (fun a b -> Float.compare (distance a) (distance b)) il

and intersects s r =
  let transform = Shape.inverse_transform s in
  let r = Ray.transform r transform in
  match Shape.geometry s with
  | Shape.Cone { min; max; capped } -> local_cone_intersects min max capped s r
  | Shape.Cube -> local_cube_intersects s r
  | Shape.Cylinder { min; max; capped } ->
      local_cylinder_intersects min max capped s r
  | Shape.Plane -> local_plane_intersects s r
  | Shape.Sphere -> local_sphere_intersects s r
  | Shape.Group sl -> local_group_intersects sl s r

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

let local_cylinder_normal_at min max _s op =
  let px = Tuple.x op and py = Tuple.y op and pz = Tuple.z op in
  let dist = (px *. px) +. (pz *. pz) in

  if dist < 1. && py >= max -. Float.epsilon then Tuple.vector 0. 1. 0.
  else if dist < 1. && py <= min +. Float.epsilon then Tuple.vector 0. (-1.) 0.
  else Tuple.vector px 0. pz

let local_cone_normal_at min max _s op =
  let px = Tuple.x op and py = Tuple.y op and pz = Tuple.z op in
  let dist = (px *. px) +. (pz *. pz) in

  if dist < max *. max && py >= max -. (Float.epsilon *. 100.) then
    Tuple.vector 0. 1. 0.
  else if dist < min *. min && py <= min +. (Float.epsilon *. 100.) then
    Tuple.vector 0. (-1.) 0.
  else
    let y = Float.sqrt dist in
    Tuple.vector px (if py > 0. then 0. -. y else y) pz

let normal_at s p =
  let pm = Tuple.to_matrix p in
  let itm = Shape.inverse_transform s in
  let object_space_p = Matrix.multiply itm pm in
  let op = Tuple.of_matrix object_space_p in

  let object_normal =
    match Shape.geometry s with
    | Shape.Cone { min; max; _ } -> local_cone_normal_at min max s op
    | Shape.Cube -> local_cube_normal_at s op
    | Shape.Cylinder { min; max; _ } -> local_cylinder_normal_at min max s op
    | Shape.Plane -> local_plane_normal_at s op
    | Shape.Sphere -> local_sphere_normal_at s op
    | Shape.Group _ -> failwith "todo"
  in

  let titm = Shape.transpose_inverse_transform s in
  let world_normal = Matrix.multiply titm (Tuple.to_matrix object_normal) in
  Tuple.normalize
    (Tuple.vector
       (Matrix.cell world_normal (0, 0))
       (Matrix.cell world_normal (1, 0))
       (Matrix.cell world_normal (2, 0)))
