open Tsdl

type tick_func = int -> Specialised.t * World.t

let ( >>= ) = Result.bind
let ( >|= ) v f = Result.map f v

let sdl_init width height title make_fullscreen =
  Sdl.init Sdl.Init.(video + events) >>= fun () ->
  Sdl.create_window ~w:width ~h:height title
    Sdl.Window.(
      (if make_fullscreen then fullscreen else windowed) + allow_highdpi)
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

let run (width, height) tick =
  let canvas = Canvas.v (width * 2, height * 2) in

  (* (* Call the first tick manually *)
  let camera_transform, world = tick 0 in
  let camera =
    Camera.v ~transform:camera_transform
      (width * 2, height * 2)
      (Float.pi *. 70. /. 180.)
  in *)
  let renderer = Render.v canvas in

  match sdl_init width height "Raybook" false with
  | Error (`Msg e) ->
      Sdl.log "Init error: %s" e;
      exit 1
  | Ok (w, r) -> (
      match
        Sdl.create_texture r Sdl.Pixel.format_rgb888 ~w:(width * 2)
          ~h:(height * 2) Sdl.Texture.access_streaming
      with
      | Error (`Msg e) ->
          Sdl.log "Texture error: %s" e;
          exit 1
      | Ok texture ->
          let rec loop counter =
            let e = Sdl.Event.create () in
            let should_quit, should_advance, should_save =
              match Sdl.poll_event (Some e) with
              | true -> (
                  match Sdl.Event.(enum (get e typ)) with
                  | `Quit -> (true, false, false)
                  | `Key_up -> (
                      let scancode = Sdl.Event.(get e keyboard_scancode) in
                      match Sdl.Scancode.enum scancode with
                      | `S -> (false, false, true)
                      | `Space -> (false, true, false)
                      | _ -> (false, false, false))
                  | _ -> (false, false, false))
              | false -> (false, counter = -1, false)
            in

            if should_save then Canvas.save_png canvas "raybook.png";

            (match
               render_texture r texture
                 (width * 2, height * 2)
                 (Canvas.raw canvas)
             with
            | Error (`Msg e) -> Sdl.log "Render error: %s" e
            | Ok () -> ());
            match should_quit with
            | true -> ()
            | false ->
                let ready = Render.is_complete renderer in

                let counter =
                  match (ready, should_advance) with
                  | true, true ->
                      let camera_transform, world = tick (counter + 1) in
                      let camera =
                        Camera.v ~transform:camera_transform
                          (width * 2, height * 2)
                          (Float.pi *. 70. /. 180.)
                      in

                      Render.render renderer camera world;
                      counter + 1
                  | _ -> counter
                in

                loop counter;
                ()
          in
          loop (-1);

          Sdl.destroy_texture texture;
          Sdl.destroy_renderer r;
          Sdl.destroy_window w;
          Sdl.quit ())
