open Raybook

(* let frame_count = 96 *)

let tick shapes _t =
  (* let ft = Float.of_int t in *)
  (* let angle = Float.pi *. 2. *. ft /. Float.of_int frame_count in *)
  let l = Light.v (Tuple.point 10. 10. 10.) (Colour.v 1. 1. 1.) in

  let plane_m =
    Material.v ~reflectivity:0.
      ~pattern:Pattern.(v (Cubes (Colour.white, Colour.v 0.5 0.5 0.5)))
      ()
  in
  let p = Shape.(v ~material:plane_m Plane) in

  let w = World.v l (p :: shapes) in

  let ct =
    Transformation.combine
      [
        Transformation.rotate_y (Float.pi /. 6.);
        Transformation.translation 0. (-0.5) (-2.);
      ]
  in

  (ct, w)

let () =
  let shapes = X3d.of_file "examples/chess/Chess/knight white.x3d" in
  let s1 = List.hd shapes in
  let a, b = Shape.bounds s1 in
  Printf.printf "%f %f %f -> %f %f %F\n" (Tuple.x a) (Tuple.y a) (Tuple.z a)  (Tuple.x b) (Tuple.y b) (Tuple.z b) ;
  let scaling = 1. /. ((Tuple.y b) -. (Tuple.y a)) in
  Printf.printf "%f\n" scaling;
  let t = Transformation.combine [
    Transformation.translation (0. -. (Tuple.x a) -. ((Tuple.x b) -. (Tuple.x a)))
    (0. -. (Tuple.y a))
    (0. -. (Tuple.z a) +. ((Tuple.z b) -. (Tuple.z a)));
    Transformation.rotate_y (Float.pi /. 4.);
    Transformation.scaling scaling scaling scaling;
    Transformation.translation (-0.25) 0. (-0.25);
  ] in
  let g = Shape.(v ~transform:t (Group [s1])) in
  Sdl.run (500, 500) (tick [g])

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
