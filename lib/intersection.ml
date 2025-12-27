type shape_t = Sphere of Sphere.t
type t = { distance : float; shape : shape_t }

let v shape distance = { shape; distance }
let distance t = t.distance
let shape t = t.shape

let sphere_intersects s r =
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
