type t = {
  distance : float;
  shape : Shape.t;
  point : Tuple.t;
  over_point : Tuple.t;
  eyev : Tuple.t;
  normalv : Tuple.t;
  inside : bool;
}

let v i r =
  let distance = Intersection.distance i in
  let shape = Intersection.shape i in
  let point = Ray.position r distance in
  let normalv = Intersection.normal_at shape point in
  let over_point =
    Tuple.add point (Tuple.multiply normalv (Float.epsilon *. 100.))
  in
  let eyev = Tuple.negate (Ray.direction r) in
  let inside, normalv =
    if Tuple.dot normalv eyev < 0. then (true, Tuple.negate normalv)
    else (false, normalv)
  in
  { distance; shape; point; over_point; eyev; normalv; inside }

let distance t = t.distance
let shape t = t.shape
let point t = t.point
let eyev t = t.eyev
let normalv t = t.normalv
let inside t = t.inside
let over_point t = t.over_point
