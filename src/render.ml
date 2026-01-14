module Chan = Domainslib.Chan

type job_t = { lineno : int; camera : Camera.t; world : World.t }
type message_t = Task of job_t | Quit

type t = {
  inputQ : message_t Chan.t;
  workers : unit Domain.t list;
  canvas : Canvas.t;
  outstanding : int Atomic.t;
}

let render_line job canvas =
  let width, _ = Canvas.dimensions canvas in
  for x_tick = 0 to width - 1 do
    let r = Camera.ray_for_pixel job.camera (x_tick, job.lineno) in
    let col = World.colour_at job.world r in
    Canvas.write_pixel canvas (x_tick, job.lineno) col
  done

let rec domain_loop inputQ outstanding canvas =
  match Chan.recv inputQ with
  | Task job ->
      render_line job canvas;
      Atomic.decr outstanding;
      domain_loop inputQ outstanding canvas
  | Quit -> Atomic.decr outstanding

let v canvas =
  let inputQ = Chan.make_unbounded () in

  let outstanding = Atomic.make 0 in

  let worker_count = Domain.recommended_domain_count () in
  let workers =
    List.init worker_count (fun _ ->
        Domain.spawn (fun _ -> domain_loop inputQ outstanding canvas))
  in

  { inputQ; workers; canvas; outstanding }

let is_complete t = Atomic.get t.outstanding = 0

let wait_for_completion t =
  while Atomic.get t.outstanding != 0 do
    Domain.cpu_relax ()
  done

let render t camera world =
  let _, height = Canvas.dimensions t.canvas in

  match Atomic.compare_and_set t.outstanding 0 height with
  | true ->
      for index = 0 to height - 1 do
        let task = { lineno = index; camera; world } in
        Chan.send t.inputQ (Task task)
      done
  | false -> failwith "attempted to start new frame before old one finished"

let teardown t =
  match Atomic.compare_and_set t.outstanding 0 (-1) with
  | true ->
      while Chan.recv_poll t.inputQ <> None do
        ()
      done;
      List.iter (fun _ -> Chan.send t.inputQ Quit) t.workers;
      List.iter (fun d -> Domain.join d) t.workers
  | false -> failwith "either busy or torn down"
