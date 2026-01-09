open Raybook

let tick t =
  let angle = Float.of_int t *. Float.pi /. 8. in

  (* if y_tick = 0 then
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        b.{x + (y * width)} <- Int32.of_int 0xFFFFFF;
      done
    done; *)
  (* if y_tick = 0 && t <> 0 then Unix.sleep 5; *)
  let c1 = Colour.v 1. 0.7 0.1 in
  let c2 = Colour.v 0.9 0.6 0.1 in
  let t =
    Matrix.multiply
      (Transformation.scaling 0.1 0.1 0.1)
      (Transformation.rotate_z (Float.pi /. 2.))
  in
  let m = Material.v ~pattern:Pattern.(v ~transform:t (Stripes (c1, c2))) () in
  let s = Shape.v ~material:m Shape.Sphere in

  let light_location = Tuple.point 0. 10. 0. in
  let t = Transformation.rotate_x angle in
  let rotated_m = Matrix.multiply t (Tuple.to_matrix light_location) in
  let rotated_p = Tuple.of_matrix rotated_m in

  let count = 18 in
  let sll : Shape.t list =
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
          Material.v ~ambient:0.2 ~reflectivity:0.9 ~diffuse:0.1 ~specular:1.
            ~shininess:300. (* ~transparency:0.9 ~refractive_index:1.5 *)
            ~pattern:Pattern.(v (Solid c))
            ()
        in
        let scale : Matrix.t = Transformation.scaling 0.15 0.15 0.15 in
        let translate : Matrix.t = Transformation.translation 1.5 0. 0. in
        let rotate_y = Transformation.rotate_y innerangle in
        let transform =
          Matrix.multiply rotate_y (Matrix.multiply translate scale)
        in
        Shape.v ~material:m ~transform Shape.Sphere)
  in

  let plane_transform =
    (* Matrix.multiply *)
    Transformation.translation 0. (-1.5) 0.
    (* (Transformation.rotate_x (Float.pi /. 10.)) *)
  in
  let plane_pattern_transform = Transformation.scaling 2. 2. 2. in
  let plane_m =
    Material.v ~reflectivity:0.5
      ~pattern:
        Pattern.(
          v ~transform:plane_pattern_transform
            (Cubes (Colour.white, Colour.v 0.5 0.5 0.5)))
      ()
  in
  let plane =
    Shape.v ~material:plane_m ~transform:plane_transform Shape.Plane
  in

  let cube_transform = Transformation.translation (-3.) (-2.) (-3.) in
  let cube_m = Material.v ~pattern:Pattern.(v (Solid Colour.white)) () in
  let cube = Shape.(v ~material:cube_m ~transform:cube_transform Cube) in

  let l = Light.v rotated_p (Colour.v 1. 1. 1.) in

  let w = World.v l (plane :: s :: cube :: sll) in

  let camera_transform =
    Matrix.multiply
      (Transformation.translation 0. 0.5 (-3.5))
      (Transformation.rotate_x 0.2)
  in
  (* let ctl =
    [
      Transformation.translation 0. 0.3 0.;
      Transformation.rotate_x (Float.pi /. 4.);
      Transformation.rotate_y (Float.pi /. 4.);
      Transformation.translation 0. (-1.5) (-2.);
    ]
  in
  let camera_transform =
    List.fold_left Matrix.multiply (Matrix.identity 4) ctl
  in *)

  (camera_transform, w)

let () = Sdl.run (500, 500) tick
