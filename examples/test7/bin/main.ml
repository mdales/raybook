open Raybook

let cols_to_cols cl = List.map (fun (r, g, b) -> Colour.v r g b) cl

let t_of_p points i =
  let a, b, c = points.(i) in
  Tuple.point a b c

let ship_to_model colours coordinates points =
  let cols = Array.of_list (cols_to_cols colours) in

  let tl =
    List.mapi
      (fun i (a, b, c) ->
        let pa = t_of_p points a
        and pb = t_of_p points b
        and pc = t_of_p points c in
        let c = cols.(i) in
        let m = Material.v ~ambient:0.3 ~pattern:Pattern.(v (Solid c)) () in
        Shape.(v ~material:m (Triangle (pa, pb, pc))))
      coordinates
  in
  Shape.(v (Group tl))

let frame_count = 96

let tick ship t =
  (* let t = 48 in *)
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
  let w = World.v l [ ship; planet ] in

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
  let colours, coordinates, points =
    Cobra3.Cobra3.(colours, coordinates, points)
  in
  (* let colours, coordinates, points = Sidewinder.Sidewinder.( colours, coordinates, points) in *)
  let ship = ship_to_model colours coordinates points in
  (* Sdl.run (500, 500) (tick ship) *)

  let c = Canvas.v (576, 324) in
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
  done
