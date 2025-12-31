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
    ~normal:(Precomputed.normalv c) ~material ~point:(Precomputed.point c) ()
