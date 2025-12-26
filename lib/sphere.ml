type t = { id : int; origin : Tuple.t }

let v id =
  let origin = Tuple.point 0. 0. 0. in
  { id; origin }

let id t = t.id

let intersects t r =
  let sphere_to_ray = Tuple.subtract (Ray.origin r) t.origin in
  let a = Tuple.dot (Ray.direction r) (Ray.direction r) in
  let b = Tuple.dot (Ray.direction r) sphere_to_ray *. 2. in
  let c = Tuple.dot sphere_to_ray sphere_to_ray -. 1. in
  let d = (b *. b) -. (4. *. a *. c) in
  match d < 0.0 with
  | true -> None
  | false ->
      Some
        ( ((b *. -1.) -. Float.sqrt d) /. (2. *. a),
          ((b *. -1.) +. Float.sqrt d) /. (2. *. a) )
