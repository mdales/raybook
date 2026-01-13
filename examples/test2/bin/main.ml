open Raybook

let tick _ =
  let count = 32 in
  let sl =
    List.init count (fun i ->
        let innerangle =
          2. *. Float.pi *. (Float.of_int i /. Float.of_int count)
        in
        let c =
          Colour.v (sin innerangle)
            (sin (innerangle +. (2. *. Float.pi /. 3.)))
            (sin (innerangle +. (4. *. Float.pi /. 3.)))
        in
        let c = Colour.fmultiply c 0.3 in
        let m =
          Material.v ~ambient:0.2 ~reflectivity:0.9 ~diffuse:0.1 ~specular:0.5
            ~shininess:300. ~transparency:0.9 ~refractive_index:1.5
            ~pattern:Pattern.(v (Solid c))
            ()
        in

        let transform = Transformation.combine [
          Transformation.scaling 0.05 10.0 0.05;
          Transformation.rotate_x (Float.pi /. 2.1);
          Transformation.translation 0. 1. 0.;
          Transformation.rotate_z innerangle;
        ] in
        Shape.v ~material:m ~transform
          (Shape.Cylinder { min = 0.; max = 1.; capped = false }))
  in

  let mt =
    Material.v ~ambient:0.1 ~pattern:Pattern.(v (Solid Colour.white)) ()
  in
  let pt = Transformation.combine [
    Transformation.rotate_x (Float.pi /. 2.);
    Transformation.translation 0. 0. (-55.);
  ] in
  let p = Shape.(v ~transform:pt ~material:mt Plane) in

  let l = Light.v (Tuple.point 0. 0. 0.) (Colour.v 1. 1. 1.) in

  let w = World.v l (p :: sl) in

  let camera_transform = Transformation.translation 0. 0.0 (-5.0) in

  (camera_transform, w)

let () = Sdl.run (500, 500) tick
