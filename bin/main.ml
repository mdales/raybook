open Raybook
open Tsdl

let ( >>= ) = Result.bind
let ( >|= ) v f = Result.map f v

let sdl_init width height title make_fullscreen =
  Sdl.init Sdl.Init.(video + events) >>= fun () ->
  Sdl.create_window ~w:width ~h:height title
    Sdl.Window.(if make_fullscreen then fullscreen else windowed)
  >>= fun w ->
  Sdl.create_renderer ~flags:Sdl.Renderer.(accelerated + presentvsync) w
  >>= fun r ->
  Sdl.show_cursor (not make_fullscreen) >|= fun _ -> (w, r)

let render_texture r texture dimensions bitmap =
  let width, height = dimensions in
  Sdl.render_clear r >>= fun () ->
  Sdl.update_texture texture None bitmap width >>= fun () ->
  let ow, oh = Result.get_ok (Sdl.get_renderer_output_size r) in
  let dst =
    Sdl.Rect.create
      ~x:((ow - width) / 2)
      ~y:((oh - height) / 2)
      ~w:width ~h:height
  in
  Sdl.render_copy ~dst r texture >|= fun () -> Sdl.render_present r

let _canvas_to_bitmap canvas bitmap dimensions =
  let width, height = dimensions in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let c = Canvas.read_pixel canvas (x, y) in
      let rgb = Colour.to_rgb c in
      bitmap.{x + (y * width)} <- rgb
    done
  done

let tick t c b =
  let width, height = Canvas.dimensions c in

  let space_width = 4.0 and space_height = 4.0 in
  let x_skip = space_width /. Float.of_int width
  and y_skip = space_height /. Float.of_int height in

  let t = t mod (width * height) in
  let y_tick = t mod height in

  let frame_key = t / height in
  let angle = Float.of_int frame_key *. Float.pi /. 8. in

  (* if y_tick = 0 then
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        b.{x + (y * width)} <- Int32.of_int 0xFFFFFF;
      done
    done; *)
  let c = Colour.v 1. 0.7 0.1 in
  let m = Material.v ~colour:c () in
  let s = Sphere.v ~material:m 42 in

  let light_location = Tuple.point (-10.) (-10.) (-10.) in
  let t = Transformation.rotate_x angle in
  let rotated_m = Matrix.multiply t (Tuple.to_matrix light_location) in
  let rotated_p = Tuple.of_matrix rotated_m in

  let l = Light.v rotated_p (Colour.v 1. 1. 1.) in

  for x_tick = 0 to width - 1 do
    let x_pos = (Float.of_int x_tick *. x_skip) -. (space_width /. 2.)
    and y_pos = (Float.of_int y_tick *. y_skip) -. (space_height /. 2.) in

    let wall_point = Tuple.point x_pos y_pos 0. in
    let camera_point = Tuple.point 0. 0. (-5.) in
    let view_vector =
      Tuple.normalize (Tuple.subtract wall_point camera_point)
    in
    let r = Ray.v camera_point view_vector in

    let il = Intersection.intersects (Shape.Sphere s) r in
    let h = Intersection.hit il in

    let col =
      match h with
      | None -> Colour.v 0. 0. 0.
      | Some t ->
          let point = Ray.position r (Intersection.distance t) in
          let normal = Intersection.normal_at (Shape.Sphere s) point in
          let eye = Tuple.negate (Ray.direction r) in

          Light.lighting ~material:m ~light:l ~point ~normal ~eye ()
    in

    let rgb = Colour.to_rgb col in
    b.{x_tick + (y_tick * width)} <- rgb
    (* Canvas.write_pixel c (x_tick, y_tick) col; *)
  done;

  true

let () =
  let width = 500 and height = 500 in

  let bitmap =
    Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (width * height)
  in
  let canvas = Canvas.v (width, height) in

  match sdl_init width height "Raybook" false with
  | Error (`Msg e) ->
      Sdl.log "Init error: %s" e;
      exit 1
  | Ok (w, r) -> (
      match
        Sdl.create_texture r Sdl.Pixel.format_rgb888 ~w:width ~h:height
          Sdl.Texture.access_streaming
      with
      | Error (`Msg e) ->
          Sdl.log "Texture error: %s" e;
          exit 1
      | Ok texture ->
          let rec loop counter =
            let e = Sdl.Event.create () in
            let should_quit =
              match Sdl.poll_event (Some e) with
              | true -> (
                  match Sdl.Event.(enum (get e typ)) with
                  | `Quit -> true
                  | _ -> false)
              | false -> false
            in

            match tick counter canvas bitmap with
            | false -> ()
            | true -> (
                (* canvas_to_bitmap canvas bitmap (width, height); *)
                (match render_texture r texture (width, height) bitmap with
                | Error (`Msg e) -> Sdl.log "Render error: %s" e
                | Ok () -> ());
                match should_quit with true -> () | false -> loop (counter + 1))
          in
          loop 0;

          Sdl.destroy_texture texture;
          Sdl.destroy_renderer r;
          Sdl.destroy_window w;
          Sdl.quit ())
