open Raybook

let tick _ =
  let c1 = Colour.v 1. 0.7 0.1 in
  (* let c2 = Colour.v 0.9 0.6 0.1 in *)
  let t =
    Specialised.multiply
      (Transformation.scaling 0.1 0.1 0.1)
      (Transformation.rotate_z (Float.pi /. 2.))
  in
  let m = Material.v ~pattern:Pattern.(v ~transform:t (Solid c1)) () in
  let t = Transformation.rotate_x (-0.5) in
  let s =
    Shape.(
      v ~material:m ~transform:t (Cone { min = 0.5; max = 1.5; capped = true }))
  in

  let mt =
    Material.v ~ambient:0.1 ~pattern:Pattern.(v (Solid Colour.white)) ()
  in
  let pt =
    List.fold_left Specialised.multiply (Specialised.identity ())
      [
        Transformation.translation 0. 0. (-55.);
        Transformation.rotate_x (Float.pi /. 2.);
      ]
  in
  let p = Shape.(v ~transform:pt ~material:mt Plane) in

  let light_location = Specialised.point 10. 1. 10. in
  let t = Transformation.rotate_x (Float.pi /. 12.) in
  let rotated_p = Specialised.multiply t light_location in
  let l = Light.v rotated_p (Colour.v 1. 1. 1.) in

  let w = World.v l [ s; p ] in

  let camera_transform =
    Specialised.multiply
      (Transformation.translation 0. 0.0 (-3.5))
      (Transformation.rotate_x 0.0)
  in

  (camera_transform, w)

let () = Sdl.run (500, 500) tick
