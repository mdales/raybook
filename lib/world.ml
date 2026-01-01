type t = { light : Light.t; shapes : Shape.t list }

let v light shapes = { light; shapes }
let light t = t.light
let shapes t = t.shapes

let intersect w r =
  List.map (fun s -> Intersection.intersects s r) w.shapes
  |> List.concat |> Intersection.sort

let shader_hit w c =
  let material = Shape.material (Precomputed.shape c) in
  Light.lighting ~light:w.light ~eye:(Precomputed.eyev c)
    ~normal:(Precomputed.normalv c) ~material ~point:(Precomputed.point c) ~shadow:false ()

let colour_at w r =
  let il = intersect w r in
  let h = Intersection.hit il in
  match h with
  | None -> Colour.v 0. 0. 0.
  | Some i ->
      let c = Precomputed.v i r in
      shader_hit w c
