open Raybook

(* let frame_count = 96 *)

let tick shapes _t =
  (* let ft = Float.of_int t in *)
  (* let angle = Float.pi *. 2. *. ft /. Float.of_int frame_count in *)
  let l = Light.v (Tuple.point 3. 5. 3.) (Colour.v 1. 1. 1.) in

  let w = World.v l shapes in

  let ct =
    Transformation.combine
      [
        (* Transformation.rotate_y (Float.pi /. 4.);
        Transformation.translation 0. (-1.75) (-12.);
        Transformation.rotate_x (Float.pi /. 4.); *)
        Transformation.rotate_x (Float.pi /. 6.);
        Transformation.translation (-3.) (2.) (-10.);
      ]
  in

  (ct, w)

let () =

  let m_white = Material.(v ~pattern:Pattern.(v (Solid Colour.white))) ()
  and m_black = Material.(v
    ~ambient:0.
    ~reflectivity:1.
    ~pattern:Pattern.(v (Solid Colour.black)) ()) in

  let pt = Transformation.translation 0. (-0.0000001) 0. in
  let p = Shape.(v ~transform:pt ~material:m_white Plane) in

  let board_tiles = List.concat (
    List.init 8 (fun y ->
      List.init 8 (fun x ->
        let m = if ((x + y) mod 2) == 0 then m_white else m_black in
        let t = Transformation.combine [
          Transformation.scaling 0.5 0.1 0.5;
          Transformation.translation ((Float.of_int x) -. 0.5) (-0.1) ((Float.of_int y) -. 0.5);
        ] in
        Shape.(v ~transform:t ~material:m Cube)
      )
    )
  ) in

  let models_files = [
    "examples/chess/Chess/rook white.x3d";
    "examples/chess/Chess/knight white.x3d";
    "examples/chess/Chess/bishop white.x3d";
    "examples/chess/Chess/king white.x3d";
    "examples/chess/Chess/queen white.x3d";
    "examples/chess/Chess/bishop white.x3d";
    "examples/chess/Chess/knight white.x3d";
    "examples/chess/Chess/rook white.x3d";
  ] in
  let models = List.mapi (fun i p ->
    let count = (List.length models_files) * 2 in
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
      Material.v ~ambient:0.2
        ~reflectivity:0.9 ~transparency:0.9 ~refractive_index:1.5
        ~diffuse:0.1 ~specular:0.5
        ~shininess:300.
        ~pattern:Pattern.(v (Solid c))
        ()
    in
    let shapes = X3d.of_file ~material:m p in
    List.hd shapes
  ) models_files in

  let scaling = List.fold_left (fun acc s ->
    let a, b = Shape.bounds s in
    let scaling = 1. /. ((Tuple.y b) -. (Tuple.y a)) in
    if scaling > acc then scaling else acc
  ) 0. models in

  let models = List.mapi (fun i s ->
    let i = Float.of_int i in
    let a, b = Shape.bounds s in
    let t = Transformation.combine [
      Transformation.translation (0. -. (Tuple.x a) -. (((Tuple.x b) -. (Tuple.x a)) /. 2.))
      (0. -. (Tuple.y a))
      (0. -. (Tuple.z a) -. (((Tuple.z b) -. (Tuple.z a)) /. 2. ));
      Transformation.scaling scaling scaling scaling;
      Transformation.rotate_y (Float.pi /. -2.);
      Transformation.translation (-0.5 +. i) 0. (-0.5);
    ] in
    Shape.(v ~transform:t (Group [s]))
  ) models in

  let pawns = List.init 8 (fun i ->
    let count = (List.length models_files) * 2 in
    let innerangle =
      2. *. Float.pi *. (Float.of_int (i + 8) /. Float.of_int count)
    in
    let c =
      Colour.v (sin innerangle)
        (sin (innerangle +. (2. *. Float.pi /. 3.)))
        (sin (innerangle +. (4. *. Float.pi /. 3.)))
    in
    let c = Colour.fmultiply c 0.3 in
    let m =
      Material.v ~ambient:0.2
        ~reflectivity:0.9 ~transparency:0.9 ~refractive_index:1.5
        ~diffuse:0.1 ~specular:0.5
        ~shininess:300.
        ~pattern:Pattern.(v (Solid c))
        ()
    in

    let shapes = X3d.of_file ~material:m "examples/chess/Chess/pawn white.x3d" in
    let s = List.hd shapes in
    let a, b = Shape.bounds s in
    let t = Transformation.combine [
      Transformation.translation (0. -. (Tuple.x a) -. (((Tuple.x b) -. (Tuple.x a)) /. 2.))
      (0. -. (Tuple.y a))
      (0. -. (Tuple.z a) -. (((Tuple.z b) -. (Tuple.z a)) /. 2. ));
      Transformation.scaling scaling scaling scaling;
      Transformation.rotate_y (Float.pi /. -2.);
      Transformation.translation (-0.5 +. (Float.of_int (7 - i))) 0. (0.5);
    ] in

    Shape.(v ~transform:t (Group [s]))
  ) in

  let team1 = Shape.(v (Group (models @ pawns))) in

  let t2t = Transformation.(combine [
    scaling (-1.) 1. 1.;
    rotate_y Float.pi;
    translation (0.) 0. (6.)
  ]) in
  let team2 = Shape.(v ~transform:t2t (Group (models @ pawns))) in

  Sdl.run (800, 500) (tick (p :: team2 :: team1 :: board_tiles))

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
