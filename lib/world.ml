type t = { light : Light.t; shapes : Shape.t list }

let v light shapes = { light; shapes }
let light t = t.light
let shapes t = t.shapes

let intersect w r =
  List.map (fun s -> Intersection.intersects s r) w.shapes
  |> List.concat |> Intersection.sort

let is_shadowed w p =
  if not (Tuple.is_point p) then
    raise (Invalid_argument "Tuple should be a point");
  let light_position = Light.position w.light in
  let v = Tuple.subtract light_position p in
  let distance = Tuple.magnitude v and direction = Tuple.normalize v in
  let r = Ray.v p direction in
  let il = intersect w r in
  match Intersection.hit il with
  | None -> false
  | Some i ->
      let t = Intersection.distance i in
      t < distance

let shader_hit w c =
  let shadow = is_shadowed w (Precomputed.over_point c) in
  let shape = Precomputed.shape c in
  let material = Shape.material shape in
  Light.lighting ~shape ~light:w.light ~eye:(Precomputed.eyev c)
    ~normal:(Precomputed.normalv c) ~material ~point:(Precomputed.point c)
    ~shadow ()

let colour_at w r =
  let il = intersect w r in
  let h = Intersection.hit il in
  match h with
  | None -> Colour.v 0. 0. 0.
  | Some i ->
      let c = Precomputed.v i r in
      shader_hit w c
