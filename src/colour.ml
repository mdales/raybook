type t = { red : float; green : float; blue : float }

let v red green blue = { red; green; blue }
let red t = t.red
let green t = t.green
let blue t = t.blue

let add t o =
  { red = t.red +. o.red; green = t.green +. o.green; blue = t.blue +. o.blue }

let subtract t o =
  { red = t.red -. o.red; green = t.green -. o.green; blue = t.blue -. o.blue }

let fmultiply t s =
  { red = t.red *. s; green = t.green *. s; blue = t.blue *. s }

let multiply t o =
  { red = t.red *. o.red; green = t.green *. o.green; blue = t.blue *. o.blue }

let fp_equal a b = abs_float (a -. b) < epsilon_float

let is_equal t o =
  fp_equal t.red o.red && fp_equal t.green o.green && fp_equal t.blue o.blue

let channel_to_byte f =
  let scaled = int_of_float (f *. 255.) in
  if scaled < 0 then 0 else if scaled > 255 then 255 else scaled

let to_rgb t =
  let red = channel_to_byte t.red
  and green = channel_to_byte t.green
  and blue = channel_to_byte t.blue in
  let res = (red lsl 16) lor (green lsl 8) lor blue in
  Int32.of_int res

let of_rgb x =
  let res = Int32.to_int x in
  let red = (res lsr 16) land 0xFF
  and green = (res lsr 8) land 0xFF
  and blue = res land 0xFF in
  {
    red = Float.of_int red /. 255.;
    green = Float.of_int green /. 255.;
    blue = Float.of_int blue /. 255.;
  }

let white = v 1. 1. 1.
let black = v 0. 0. 0.
(* let red = v 1. 0. 0. *)
(* let green = v 0. 1. 0.
let blue = v 0. 0. 1. *)
