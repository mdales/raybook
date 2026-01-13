open Raybook

let gen_group (t : Matrix.t) =
    let body = Shape.(v (Cylinder {min=(-1.);max=1.;capped=false})) in
    let s1 = Shape.(v ~transform:(Transformation.translation 0. 1. 0.) Sphere) in
    let s2 = Shape.(v ~transform:(Transformation.translation 0. (-1.) 0.) Sphere) in
    Shape.(v ~transform:t (Group [s1 ; s2; body]))

let tick _ =

    let sl = List.init 5 (fun y ->
        List.init 5 (fun x ->
            let fx = Float.of_int x
            and fy = Float.of_int y in
            let t = Transformation.translation (fx *. 4.) (fy *. 8) 0. in
            gen_group t
        )
    ) |> List.concat in

  let mt =
    Material.v ~ambient:0.1 ~pattern:Pattern.(v (Solid Colour.white)) ()
  in
  let pt =
    List.fold_left Matrix.multiply (Matrix.identity 4)
      [
        Transformation.translation 0. 0. (-0.);
        Transformation.rotate_x (Float.pi /. 2.);
      ]
  in
  let p = Shape.(v ~transform:pt ~material:mt Plane) in

  let light_location = Tuple.point 10. 1. 50. in
  let t = Transformation.rotate_x (Float.pi /. 12.) in
  let rotated_m = Matrix.multiply t (Tuple.to_matrix light_location) in
  let rotated_p = Tuple.of_matrix rotated_m in
  let l = Light.v rotated_p (Colour.v 1. 1. 1.) in

  let w = World.v l (p :: sl) in

  let camera_transform =
    Matrix.multiply
      (Transformation.translation 0. 0.0 (-13.5))
      (Transformation.rotate_x 0.0)
  in

  (camera_transform, w)

let () = Sdl.run (500, 500) tick
