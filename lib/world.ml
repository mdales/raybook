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

let max_reflection_depth = 5

let rec shader_hit ?(count = max_reflection_depth) w c =
  let shadow = is_shadowed w (Precomputed.over_point c) in
  let shape = Precomputed.shape c in
  let material = Shape.material shape in
  let surface =
    Light.lighting ~shape ~light:w.light ~eye:(Precomputed.eyev c)
      ~normal:(Precomputed.normalv c) ~material ~point:(Precomputed.point c)
      ~shadow ()
  in
  match count with
  | 0 -> surface
  | _ ->
      let reflected = reflected_colour ~count:(count - 1) w c in
      let refracted = refracted_colour ~count:(count - 1) w c in
      Colour.add (Colour.add surface reflected) refracted

and colour_at ?(count = max_reflection_depth) w r =
  let il = intersect w r in
  let h = Intersection.hit il in
  match h with
  | None -> Colour.v 0. 0. 0.
  | Some i ->
      let c = Precomputed.v i r il in
      shader_hit ~count w c

and reflected_colour ?(count = 0) w c =
  let m = Shape.material (Precomputed.shape c) in
  let r = Material.reflectivity m in
  match r with
  | 0. -> Colour.black
  | _ ->
      let reflect_ray =
        Ray.v (Precomputed.over_point c) (Precomputed.reflectv c)
      in
      let col = colour_at ~count w reflect_ray in
      Colour.fmultiply col r

and refracted_colour ?(count = 1) w c =
  match count with
  | 0 -> Colour.black
  | _ -> (
      let m = Shape.material (Precomputed.shape c) in
      let r = Material.transparency m in
      match r with
      | 0. -> Colour.black
      | _ ->
          let n1, n2 = Precomputed.n_pair c in
          let n_ratio = n1 /. n2 in
          let cos_i = Tuple.dot (Precomputed.eyev c) (Precomputed.normalv c) in
          let sin2_t = n_ratio *. n_ratio *. (1. -. (cos_i *. cos_i)) in
          if sin2_t > 1. then Colour.black
          else
            let cos_t = Float.sqrt (1. -. sin2_t) in
            let direction =
              Tuple.subtract
                (Tuple.multiply (Precomputed.normalv c)
                   ((n_ratio *. cos_i) -. cos_t))
                (Tuple.multiply (Precomputed.eyev c) n_ratio)
            in
            let refracted_ray = Ray.v (Precomputed.under_point c) direction in
            let col = colour_at ~count w refracted_ray in
            Colour.fmultiply col r)
