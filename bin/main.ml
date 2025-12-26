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

let canvas_to_bitmap canvas bitmap dimensions =
  let width, height = dimensions in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let c = Canvas.read_pixel canvas (x, y) in
      let rgb = Colour.to_rgb c in
      bitmap.{x + (y * width)} <- rgb
    done
  done

let tick t c =
  let width, height = Canvas.dimensions c in

  let p = Tuple.point 0. 0. 0. in
  let angle = ((Float.of_int t) /. 720.) *. Float.pi in
  let r = Transformation.rotate_z angle in
  let scale = Transformation.translation 0. 100. 0. in
  let t = Matrix.multiply r scale in
  let final = Matrix.multiply t (Tuple.to_matrix p) in
  let finalp = Tuple.of_matrix final in

  let xpos = Int.of_float (Tuple.x finalp)
  and ypos = Int.of_float (Tuple.y finalp) in

  Canvas.write_pixel c ((width / 2) + xpos, (height / 2) + ypos) (Colour.v 1. 1. 1.);

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

            match tick counter canvas with
            | false -> ()
            | true -> (
                canvas_to_bitmap canvas bitmap (width, height);
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
