open Tsdl

type tick_func = int -> Matrix.t * World.t

let ( >>= ) = Result.bind
let ( >|= ) v f = Result.map f v

type state = { camera : Camera.t; world : World.t }

let inner_tick tickf s t c =
  let width, height = Canvas.dimensions c in

  let t = t mod (width * height) in
  let y_tick = t mod height in

  let frame_key = t / height in

  let tick_state =
    match y_tick == 0 with
    | false -> s
    | true ->
        let camera_transform, world = tickf frame_key in
        let camera =
          Camera.v ~transform:camera_transform (width, height)
            (Float.pi *. 70. /. 180.)
        in
        { camera; world }
  in

  for x_tick = 0 to width - 1 do
    let r = Camera.ray_for_pixel tick_state.camera (x_tick, y_tick) in
    let col = World.colour_at tick_state.world r in
    Canvas.write_pixel c (x_tick, y_tick) col
  done;

  tick_state

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

  (* Call the first tick manually *)
  let camera_transform, world = tick 0 in
  let camera =
    Camera.v ~transform:camera_transform (width, height)
      (Float.pi *. 70. /. 180.)
  in
  let init_state = { camera; world } in

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
          let rec loop counter render_state =
            let e = Sdl.Event.create () in
            let should_quit =
              match Sdl.poll_event (Some e) with
              | true -> (
                  match Sdl.Event.(enum (get e typ)) with
                  | `Quit -> true
                  | _ -> false)
              | false -> false
            in

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
                let next_state = inner_tick tick render_state counter canvas in
                loop (counter + 1) next_state
          in
          loop 0 init_state;

          Sdl.destroy_texture texture;
          Sdl.destroy_renderer r;
          Sdl.destroy_window w;
          Sdl.quit ())
