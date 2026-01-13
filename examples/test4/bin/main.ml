open Raybook

let gen_group m t =
  let body =
    Shape.(v ~material:m (Cylinder { min = -1.; max = 1.; capped = false }))
  in
  let s1 =
    Shape.(
      v ~material:m ~transform:(Transformation.translation 0. 1. 0.) Sphere)
  in
  let s2 =
    Shape.(
      v ~material:m ~transform:(Transformation.translation 0. (-1.) 0.) Sphere)
  in
  Shape.(v ~transform:t (Group [ s1; s2; body ]))

let tick _ =
  let mt =
    Material.v ~ambient:0.15 ~pattern:Pattern.(v (Solid Colour.white)) ()
  in

  let sl =
    List.init 7 (fun y ->
        let il =
          List.init 9 (fun x ->
              let fx = Float.of_int x in
              let t = Transformation.translation ((fx *. 4.) -. 12.) 0. 0. in

              let mt =
                match (x, y) with
                | 1, 5 ->
                    Material.v ~ambient:0.15
                      ~pattern:Pattern.(v (Solid (Colour.v 1. 0.3 0.3)))
                      ()
                | 3, 4 ->
                    Material.v ~ambient:0.15
                      ~pattern:Pattern.(v (Solid (Colour.v 0.3 1.0 0.3)))
                      ()
                | 5, 3 ->
                    Material.v ~ambient:0.15
                      ~pattern:Pattern.(v (Solid (Colour.v 0.3 0.3 1.0)))
                      ()
                | _ -> mt
              in
              gen_group mt t)
        in
        let fy = Float.of_int y in
        let t = Transformation.translation 0. ((fy *. 6.) -. 24.) 0. in
        Shape.(v ~transform:t (Group il)))
  in

  let pt =
    List.fold_left Matrix.multiply (Matrix.identity 4)
      [
        Transformation.translation 0. 0. (-0.);
        Transformation.rotate_x (Float.pi /. 2.);
      ]
  in
  let p = Shape.(v ~transform:pt ~material:mt Plane) in

  let light_location = Tuple.point 10. 0. 50. in
  let t = Transformation.rotate_x (Float.pi /. 12.) in
  let rotated_m = Matrix.multiply t (Tuple.to_matrix light_location) in
  let rotated_p = Tuple.of_matrix rotated_m in
  let l = Light.v rotated_p (Colour.v 1. 1. 1.) in

  let w = World.v l (p :: sl) in

  let camera_transform =
    Matrix.multiply
      (Transformation.translation 0. 0.0 (-20.))
      (Transformation.rotate_x 0.0)
  in

  (camera_transform, w)

let () = Sdl.run (500, 500) tick
