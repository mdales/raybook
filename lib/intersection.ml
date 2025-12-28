type shape_t = Sphere of Sphere.t
type t = { distance : float; shape : shape_t }

let v shape distance = { shape; distance }
let distance t = t.distance
let shape t = t.shape

let sphere_intersects s r =
  let transform = Sphere.inverse_transform s in
  let r = Ray.transform r transform in
  let sphere_to_ray = Tuple.subtract (Ray.origin r) (Tuple.point 0. 0. 0.) in
  let a = Tuple.dot (Ray.direction r) (Ray.direction r) in
  let b = Tuple.dot (Ray.direction r) sphere_to_ray *. 2. in
  let c = Tuple.dot sphere_to_ray sphere_to_ray -. 1. in
  let d = (b *. b) -. (4. *. a *. c) in
  match d < 0.0 with
  | true -> []
  | false ->
      [
        v (Sphere s) (((b *. -1.) -. Float.sqrt d) /. (2. *. a));
        v (Sphere s) (((b *. -1.) +. Float.sqrt d) /. (2. *. a));
      ]

let intersects s r = match s with Sphere s -> sphere_intersects s r

let hit tl =
  match tl with
  | [] -> None
  | tl ->
      (* In future we should just ensure this is sorted always *)
      let sorted_lt =
        List.sort (fun a b -> Float.compare a.distance b.distance) tl
      in
      let rec loop tl =
        match tl with
        | [] -> None
        | x :: xs ->
            let d = x.distance in
            if d >= 0. then Some x else loop xs
      in
      loop sorted_lt

let sphere_normal_at _s p =
  Tuple.normalize (Tuple.subtract p (Tuple.point 0. 0. 0.))

let normal_at s p = match s with Sphere s -> sphere_normal_at s p
