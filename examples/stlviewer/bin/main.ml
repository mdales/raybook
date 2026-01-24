open Raybook

(* let frame_count = 96 *)

let tick shapes _t =
  (* let ft = Float.of_int t in *)
  (* let angle = Float.pi *. 2. *. ft /. Float.of_int frame_count in *)
  let l = Light.v (Specialised.point 10. 10. 10.) (Colour.v 1. 1. 1.) in

  let plane_m =
    Material.v ~reflectivity:0.5
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
        Transformation.rotate_x 0.4;
        Transformation.translation 0. (-0.5) (0.);
      ]
  in

  (ct, w)

let stl_to_ray stl_file =
    let triangles = Stl.triangles stl_file in
    let shapes = List.map (fun t ->
        let v1, v2, v3 = Stl.Triangle.vertices t in
        let x, y, z = v1 in
        let p1 = Specialised.point x y z in
        let x, y, z = v2 in
        let p2 = Specialised.point x y z in
        let x, y, z = v3 in
        let p3 = Specialised.point x y z in

        let colbits = Stl.Triangle.attribute_bytes t in
        let b = (Float.of_int ((colbits lsr 10) land 0x1F)) /. 31. in
        let g = (Float.of_int ((colbits lsr 5) land 0x1F)) /. 31. in
        let r = (Float.of_int ((colbits lsr 0) land 0x1F)) /. 31. in
        let c = Colour.v r g b in
        let p = Pattern.(v (Solid c)) in
        let m = Material.(v ~pattern:p) () in
        let s = Shape.(v ~material:m (Triangle (p1, p2, p3))) in
        s
    ) triangles in

    (* this is stupid, we need a better API here. I make a group
    just to get the bounds then make another group with the transform *)
    let g = Shape.(v (Group shapes)) in
    let minm, maxm = Shape.bounds g in
    let xdiff = (Specialised.x maxm) -. (Specialised.x minm)
    and ydiff = (Specialised.y maxm) -. (Specialised.y minm)
    and zdiff = (Specialised.z maxm) -. (Specialised.z minm) in
    let maxdiff = max (max xdiff ydiff) zdiff in
    let scale = 1. /. maxdiff in
    let t = Transformation.scaling scale scale scale in
    let g = Shape.(v ~transform:t (Group shapes)) in
    g

let () =
  match (Stl.of_file (Sys.argv.(1))) with
  | Result.Error msg -> failwith msg
  | Result.Ok stl_file -> (
    let s = stl_to_ray stl_file in
    let shapes = [s] in
    Sdl.run (500, 500) (tick shapes)
)

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
