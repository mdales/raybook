open Raybook

let frame_count = 96

let tick t =
  let count = 9 in
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

        let t =
          Transformation.combine
            [
              Transformation.scaling 0.5 1. 0.5;
              Transformation.translation
                (Float.of_int (i - (count / 2)) *. 2.)
                0. 0.;
            ]
        in

        Shape.(v ~transform:t ~material:m Cube))
  in

  let mt =
    Material.v ~ambient:0.2 ~reflectivity:0. ~diffuse:0.7 ~specular:0.
      ~pattern:Pattern.(v (Solid (Colour.v 1. 1. 1.)))
      ()
  in
  let pt =
    List.fold_left Matrix.multiply (Matrix.identity 4)
      [ Transformation.translation 0. (-1.) 0. ]
  in
  let p = Shape.(v ~transform:pt ~material:mt Plane) in

  let ft = Float.of_int t and fc = Float.of_int frame_count in
  let light_angle = Float.pi *. 2. *. ft /. fc in
  let intensity = 1. in
  (* ((cos light_angle) *. 0.7) +. 0.3 in *)
  let lp = Tuple.point 0. 4. (-2.) in
  let lpm = Tuple.to_matrix lp in
  let light_transform =
    Transformation.combine
      [
        (* Transformation.translation 0. 0. ((sin light_angle) *. 3.); *)
        Transformation.rotate_y light_angle;
        Transformation.scaling 4. 1. 1.;
      ]
  in
  let ulpm = Matrix.multiply light_transform lpm in
  let ulp = Tuple.of_matrix ulpm in

  let l = Light.v ulp (Colour.v intensity intensity intensity) in

  let w = World.v l (p :: sl) in

  let camera_transform =
    Transformation.combine
      [ Transformation.rotate_x 0.4; Transformation.translation 0. 0.5 (-13.5) ]
  in

  (camera_transform, w)

let () =
  let c =  Canvas.v (576 * 2, 324 * 2) in
  let r = Render.v c in
  for idx = 0 to frame_count - 1 do
    let ct, world = tick idx in
    let camera =
      Camera.v ~transform:ct (Canvas.dimensions c) (Float.pi *. 70. /. 180.)
    in
    Render.render r camera world;
    Render.wait_for_completion r;
    let filename = Printf.sprintf "frame_%03d.png" idx in
    Canvas.save_png c filename
  done
