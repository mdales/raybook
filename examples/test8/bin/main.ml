open Raybook

(* let frame_count = 96 *)

let tick shapes _t =
  (* let ft = Float.of_int t in *)
  (* let angle = Float.pi *. 2. *. ft /. Float.of_int frame_count in *)
  let l = Light.v (Tuple.point 10. 10. 10.) (Colour.v 1. 1. 1.) in

  let plane_m =
    Material.v ~reflectivity:0.5
      ~pattern:
        Pattern.(
          v
            (Cubes (Colour.white, Colour.v 0.5 0.5 0.5)))
      ()
  in
  let p = Shape.(v ~material:plane_m Plane) in


  let w = World.v l (p :: shapes) in

  let ct =
    Transformation.combine [
    Transformation.rotate_y (Float.pi /. 6.);
    Transformation.translation 0. (-0.5) (-2.)
    ]
  in

  (ct, w)

let () =
  let shapes = X3d.of_file "Teapot.x3d" in
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
