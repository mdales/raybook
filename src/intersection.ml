type t = { distance : float; shape : Shape.t }

let v shape distance = { shape; distance }
let distance t = t.distance
let shape t = t.shape

let cube_axis_check ?(lower = -1.) ?(upper = 1.) o d =
  let tmin_enum = lower -. o and tmax_enum = upper -. o in
  let tmin, tmax =
    if Float.abs d > Float.epsilon then (tmin_enum /. d, tmax_enum /. d)
    else
      (* this gives them the correct sign *)
      (tmin_enum *. Float.infinity, tmax_enum *. Float.infinity)
  in
  if tmin > tmax then (tmax, tmin) else (tmin, tmax)

let bounds_intersects s r =
  let minb, maxb = Shape.bounds s in
  let o = Ray.origin r and d = Ray.direction r in
  let xmin, xmax =
    cube_axis_check ~lower:(Specialised.x minb) ~upper:(Specialised.x maxb)
      (Specialised.x o) (Specialised.x d)
  and ymin, ymax =
    cube_axis_check ~lower:(Specialised.y minb) ~upper:(Specialised.y maxb)
      (Specialised.y o) (Specialised.y d)
  and zmin, zmax =
    cube_axis_check ~lower:(Specialised.z minb) ~upper:(Specialised.z maxb)
      (Specialised.z o) (Specialised.z d)
  in
  let tmin = Float.max (Float.max xmin ymin) zmin
  and tmax = Float.min (Float.min xmax ymax) zmax in
  if tmin < tmax then true else false

let local_sphere_intersects s r =
  let sphere_to_ray =
    Specialised.subtract (Ray.origin r) (Specialised.point 0. 0. 0.)
  in
  let a = Specialised.dot (Ray.direction r) (Ray.direction r) in
  let b = Specialised.dot (Ray.direction r) sphere_to_ray *. 2. in
  let c = Specialised.dot sphere_to_ray sphere_to_ray -. 1. in
  let d = (b *. b) -. (4. *. a *. c) in
  match d < 0.0 with
  | true -> []
  | false ->
      [
        v s (((b *. -1.) -. Float.sqrt d) /. (2. *. a));
        v s (((b *. -1.) +. Float.sqrt d) /. (2. *. a));
      ]

let local_plane_intersects s r =
  let direction_y = Specialised.y (Ray.direction r) in
  if Float.abs direction_y < Float.epsilon then []
  else
    let origin_y = Specialised.y (Ray.origin r) in
    let t = (0. -. origin_y) /. direction_y in
    [ v s t ]

let local_cube_intersects s r =
  let o = Ray.origin r and d = Ray.direction r in
  let xmin, xmax = cube_axis_check (Specialised.x o) (Specialised.x d)
  and ymin, ymax = cube_axis_check (Specialised.y o) (Specialised.y d)
  and zmin, zmax = cube_axis_check (Specialised.z o) (Specialised.z d) in
  let tmin = Float.max (Float.max xmin ymin) zmin
  and tmax = Float.min (Float.min xmax ymax) zmax in
  if tmin < tmax then [ v s tmin; v s tmax ] else []

let check_cap r t radius =
  let o = Ray.origin r in
  let d = Ray.direction r in
  let x = Specialised.x o +. (t *. Specialised.x d)
  and z = Specialised.z o +. (t *. Specialised.z d) in
  (x *. x) +. (z *. z) <= radius *. radius

let intersect_caps min max capped s r il is_cone =
  match capped with
  | false -> il
  | true -> (
      let dy = Specialised.y (Ray.direction r) in
      match Float.abs dy < Float.epsilon with
      | true -> il
      | false -> (
          let oy = Specialised.y (Ray.origin r) in
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
  let dx = Specialised.x d and dz = Specialised.z d in
  let a = (dx *. dx) +. (dz *. dz) in
  let il =
    match Float.abs a < Float.epsilon with
    | true -> []
    | false -> (
        let o = Ray.origin r in
        let ox = Specialised.x o and oz = Specialised.z o in
        let b = (2. *. ox *. dx) +. (2. *. oz *. dz)
        and c = (ox *. ox) +. (oz *. oz) -. 1. in
        let disc = (b *. b) -. (4. *. a *. c) in
        match disc < 0. with
        | true -> []
        | false -> (
            let t0 = ((b *. -1.) -. Float.sqrt disc) /. (2. *. a)
            and t1 = ((b *. -1.) +. Float.sqrt disc) /. (2. *. a) in
            let t0, t1 = if t0 > t1 then (t1, t0) else (t0, t1) in

            let y0 = Specialised.y o +. (t0 *. Specialised.y d)
            and y1 = Specialised.y o +. (t1 *. Specialised.y d) in

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
  let dx = Specialised.x d and dy = Specialised.y d and dz = Specialised.z d in
  let o = Ray.origin r in
  let ox = Specialised.x o and oy = Specialised.y o and oz = Specialised.z o in
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

        let y0 = Specialised.y o +. (t0 *. Specialised.y d)
        and y1 = Specialised.y o +. (t1 *. Specialised.y d) in

        let y0in = y0 > min && y0 < max and y1in = y1 > min && y1 < max in
        match (y0in, y1in) with
        | false, false -> []
        | true, false -> [ v s t0 ]
        | false, true -> [ v s t1 ]
        | true, true -> [ v s t0; v s t1 ])
  in

  intersect_caps min max capped s r il true

let local_triangle_intersects (p1, _, _) s r =
  let e1, e2 = Option.get (Shape.edges s) in
  let d = Ray.direction r in
  let dir_cross_e2 = Specialised.cross d e2 in
  let det = Specialised.dot e1 dir_cross_e2 in
  if Float.abs det < Float.epsilon then []
  else
    let f = 1. /. det in
    let o = Ray.origin r in
    let p1_to_origin = Specialised.subtract o p1 in
    let u = f *. Specialised.dot p1_to_origin dir_cross_e2 in
    if u < 0. || u > 1. then []
    else
      let origin_cross_e1 = Specialised.cross p1_to_origin e1 in
      let vv = f *. Specialised.dot d origin_cross_e1 in
      if vv < 0. || vv +. u > 1. then []
      else
        let t = f *. Specialised.dot e2 origin_cross_e1 in
        [ v s t ]

let rec intersects s r =
  match Shape.geometry s with
  | Shape.Group sl -> (
      match bounds_intersects s r with
      | false -> []
      | true ->
          (* Groups have their child shapes in world space *)
          List.map (fun is -> intersects is r) sl
          |> List.concat
          |> List.sort (fun a b -> Float.compare (distance a) (distance b)))
  | _ -> (
      let transform = Shape.inverse_transform s in
      let r = Ray.transform r transform in
      match Shape.geometry s with
      | Shape.Cone { min; max; capped } ->
          local_cone_intersects min max capped s r
      | Shape.Cube -> local_cube_intersects s r
      | Shape.Cylinder { min; max; capped } ->
          local_cylinder_intersects min max capped s r
      | Shape.Plane -> local_plane_intersects s r
      | Shape.Sphere -> local_sphere_intersects s r
      | Shape.Triangle c -> local_triangle_intersects c s r
      | Shape.Group _ -> failwith "should not be reached")

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

let local_sphere_normal_at _s op =
  Specialised.subtract op (Specialised.point 0. 0. 0.)

let local_plane_normal_at _s _op = Specialised.vector 0. 1. 0.

let local_cube_normal_at _s op =
  let abs_x = Float.abs (Specialised.x op)
  and abs_y = Float.abs (Specialised.y op)
  and abs_z = Float.abs (Specialised.z op) in
  let maxc = Float.max (Float.max abs_x abs_y) abs_z in

  if abs_x = maxc then Specialised.vector (Specialised.x op) 0. 0.
  else if abs_y = maxc then Specialised.vector 0. (Specialised.y op) 0.
  else Specialised.vector 0. 0. (Specialised.z op)

let local_cylinder_normal_at min max _s op =
  let px = Specialised.x op
  and py = Specialised.y op
  and pz = Specialised.z op in
  let dist = (px *. px) +. (pz *. pz) in

  if dist < 1. && py >= max -. Float.epsilon then Specialised.vector 0. 1. 0.
  else if dist < 1. && py <= min +. Float.epsilon then
    Specialised.vector 0. (-1.) 0.
  else Specialised.vector px 0. pz

let local_cone_normal_at min max _s op =
  let px = Specialised.x op
  and py = Specialised.y op
  and pz = Specialised.z op in
  let dist = (px *. px) +. (pz *. pz) in

  if dist < max *. max && py >= max -. (Float.epsilon *. 100.) then
    Specialised.vector 0. 1. 0.
  else if dist < min *. min && py <= min +. (Float.epsilon *. 100.) then
    Specialised.vector 0. (-1.) 0.
  else
    let y = Float.sqrt dist in
    Specialised.vector px (if py > 0. then 0. -. y else y) pz

let normal_at s p =
  let itm = Shape.inverse_transform s in
  let op = Specialised.multiply itm p in

  let object_normal =
    match Shape.geometry s with
    | Shape.Cone { min; max; _ } -> local_cone_normal_at min max s op
    | Shape.Cube -> local_cube_normal_at s op
    | Shape.Cylinder { min; max; _ } -> local_cylinder_normal_at min max s op
    | Shape.Plane -> local_plane_normal_at s op
    | Shape.Sphere -> local_sphere_normal_at s op
    | Shape.Triangle _ -> Option.get (Shape.normal s)
    | Shape.Group _ -> failwith "should not happen"
  in

  let titm = Shape.transpose_inverse_transform s in
  let world_normal = Specialised.multiply titm object_normal in
  Specialised.normalize
    (Specialised.vector
       (Specialised.cell world_normal (0, 0))
       (Specialised.cell world_normal (1, 0))
       (Specialised.cell world_normal (2, 0)))
