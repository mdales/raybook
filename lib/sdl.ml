open Tsdl
module Chan = Domainslib.Chan

type message = Task of int | Done of int | Quit
type tick_func = int -> Matrix.t * World.t

let ( >>= ) = Result.bind
let ( >|= ) v f = Result.map f v

type state = { camera : Camera.t; world : World.t }

let inner_tick lineno state c =
  let width, _ = Canvas.dimensions c in

  for x_tick = 0 to width - 1 do
    let r = Camera.ray_for_pixel state.camera (x_tick, lineno) in
    let col = World.colour_at state.world r in
    Canvas.write_pixel c (x_tick, lineno) col
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

let rec domain_loop state inputQ outputQ canvas =
  match Chan.recv inputQ with
  | Task lineno ->
      inner_tick lineno !state canvas;
      Chan.send outputQ (Done lineno);
      domain_loop state inputQ outputQ canvas
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
  let state = ref { camera; world } in

  let worker_count = Domain.recommended_domain_count () in
  let inputQ = Chan.make_unbounded () in
  let outputQ = Chan.make_unbounded () in
  let _workers =
    List.init worker_count (fun _ ->
        Domain.spawn (fun _ -> domain_loop state inputQ outputQ canvas))
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
          let rec loop outstanding_blocks counter =
            let e = Sdl.Event.create () in
            let should_quit, should_advance =
              match Sdl.poll_event (Some e) with
              | true -> (
                  match Sdl.Event.(enum (get e typ)) with
                  | `Quit -> (true, false)
                  | `Key_up -> (false, true)
                  | _ -> (false, false))
              | false -> (false, counter = -1)
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
                let rec recvloop acc =
                  match Chan.recv_poll outputQ with
                  | None -> acc
                  | Some msg -> (
                      match msg with Done _ -> recvloop (acc - 1) | _ -> acc)
                in
                let outstanding_blocks = recvloop outstanding_blocks in

                let outstanding_blocks, counter =
                  match (outstanding_blocks, should_advance) with
                  | 0, true ->
                      let camera_transform, world = tick (counter + 1) in
                      let camera =
                        Camera.v ~transform:camera_transform
                          (width * 2, height * 2)
                          (Float.pi *. 70. /. 180.)
                      in
                      state := { camera; world };
                      for index = 0 to (height * 2) - 1 do
                        Chan.send inputQ (Task index)
                      done;
                      (height * 2, counter + 1)
                  | _ -> (outstanding_blocks, counter)
                in

                loop outstanding_blocks counter;
                ()
          in
          loop 0 (-1);

          Sdl.destroy_texture texture;
          Sdl.destroy_renderer r;
          Sdl.destroy_window w;
          Sdl.quit ())
