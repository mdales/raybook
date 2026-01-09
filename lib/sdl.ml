open Tsdl
module Chan = Domainslib.Chan

type 'a message = Task of 'a | Done of int | Quit
type tick_func = int -> Matrix.t * World.t

let ( >>= ) = Result.bind
let ( >|= ) v f = Result.map f v

type state = { camera : Camera.t; world : World.t }

let inner_tick linestart linecount tick_state c =
  let width, height = Canvas.dimensions c in

  let blockheight = min height (linestart + linecount) in
  let rows = blockheight - linestart in

  for y_tick = linestart to linestart + rows - 1 do
    for x_tick = 0 to width - 1 do
      let r = Camera.ray_for_pixel tick_state.camera (x_tick, y_tick) in
      let col = World.colour_at tick_state.world r in
      Canvas.write_pixel c (x_tick, y_tick) col
    done
  done

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

let rec domain_loop index worker_count inputQ outputQ canvas =
  match Chan.recv inputQ with
  | Task state ->
      let _, h = Canvas.dimensions canvas in
      let step = h / worker_count in
      let linestart = index * step and linecount = step in
      inner_tick linestart linecount state canvas;
      Chan.send outputQ (Done linestart);
      domain_loop index worker_count inputQ outputQ canvas
  | Done _ -> ()
  | Quit -> Chan.send outputQ Quit

let run (width, height) tick =
  let canvas = Canvas.v (width * 2, height * 2) in

  (* Call the first tick manually *)
  let camera_transform, world = tick 0 in
  let camera =
    Camera.v ~transform:camera_transform
      (width * 2, height * 2)
      (Float.pi *. 70. /. 180.)
  in
  let init_state = { camera; world } in

  let worker_count = Domain.recommended_domain_count () in
  let inputQ = Chan.make_unbounded () in
  let outputQ = Chan.make_unbounded () in
  let _workers =
    List.init worker_count (fun i ->
        let domain =
          Domain.spawn (fun _ ->
              domain_loop i worker_count inputQ outputQ canvas)
        in
        Chan.send inputQ (Task init_state);
        domain)
  in

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
          let rec loop outstanding_workers counter =
            let e = Sdl.Event.create () in
            let should_quit, should_advance =
              match Sdl.poll_event (Some e) with
              | true -> (
                  match Sdl.Event.(enum (get e typ)) with
                  | `Quit -> (true, false)
                  | `Key_up -> (false, true)
                  | _ -> (false, false))
              | false -> (false, false)
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
                let outstanding_workers =
                  match Chan.recv_poll outputQ with
                  | None -> outstanding_workers
                  | Some msg -> (
                      match msg with
                      | Done _ -> outstanding_workers - 1
                      | _ -> outstanding_workers)
                in

                let outstanding_workers, counter =
                  match (outstanding_workers, should_advance) with
                  | 0, true ->
                      let camera_transform, world = tick (counter + 1) in
                      let camera =
                        Camera.v ~transform:camera_transform
                          (width * 2, height * 2)
                          (Float.pi *. 70. /. 180.)
                      in
                      let new_state = { camera; world } in
                      for _ = 0 to worker_count - 1 do
                        Chan.send inputQ (Task new_state)
                      done;
                      (worker_count, counter + 1)
                  | _ -> (outstanding_workers, counter)
                in

                loop outstanding_workers counter;
                ()
          in
          loop worker_count 0;

          Sdl.destroy_texture texture;
          Sdl.destroy_renderer r;
          Sdl.destroy_window w;
          Sdl.quit ())
