open Raybook

let frame_count = 96

let tick ships t =
  let ft = Float.of_int t in
  let angle = Float.pi *. 2. *. ft /. Float.of_int frame_count in

  let c1 = Colour.v 1. 0.7 0.1 in
  let c2 = Colour.v 0.9 0.6 0.1 in
  let t =
    Matrix.multiply
      (Transformation.scaling 0.1 0.1 0.1)
      (Transformation.rotate_z (Float.pi /. 2.))
  in
  let tp =
    Transformation.combine
      [
        Transformation.scaling 240. 240. 240.;
        (* Transformation.rotate_z (Float.pi /. 2.); *)
        Transformation.translation 300. (-300.) 00.;
      ]
  in
  let m = Material.v ~pattern:Pattern.(v ~transform:t (Stripes (c1, c2))) () in
  let planet = Shape.(v ~transform:tp ~material:m Sphere) in

  let l = Light.v (Tuple.point 100. 100. 100.) (Colour.v 1. 1. 1.) in
  let w = World.v l (planet :: ships) in

  let ct =
    Transformation.combine
      [
        Transformation.rotate_x (Float.pi /. 10.);
        Transformation.rotate_y angle;
        (*(Float.pi /. 8.);*)
        Transformation.translation 0. 0. (-200.);
      ]
  in

  (ct, w)

let () =
  let shapes = X3d.of_file "cobra3.x3d" in
  Sdl.run (500, 500) (tick shapes)

(* let c = Canvas.v (576, 324) in
  let r = Render.v c in
  for idx = 0 to frame_count - 1 do
    let ct, world = tick ship idx in
    let camera =
      Camera.v ~transform:ct (Canvas.dimensions c) (Float.pi *. 70. /. 180.)
    in
    Render.render r camera world;
    Render.wait_for_completion r;
    let filename = Printf.sprintf "frame_%02d.png" idx in
    Canvas.save_png c filename
  done *)
