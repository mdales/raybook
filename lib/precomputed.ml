type t = {
  distance : float;
  shape : Shape.t;
  point : Tuple.t;
  over_point : Tuple.t;
  eyev : Tuple.t;
  normalv : Tuple.t;
  reflectv : Tuple.t;
  inside : bool;
  n1 : float;
  n2 : float;
}

let calc_n1_n2 h il =
  match il with
  | [] -> (1., 1.) (* shouldn't happen, but just in case *)
  | i :: [] ->
      (* this is a common case for planes *)
      if i <> h then failwith "Expected hit to be in intersection list";
      let m = Shape.material (Intersection.shape i) in
      (1., Material.refractive_index m)
  | [ a; b ] ->
      if a = h then
        let m = Shape.material (Intersection.shape a) in
        (1., Material.refractive_index m)
      else (
        if b <> h then
          failwith "Expected hit to be one of two intersection list";
        if Intersection.shape a = Intersection.shape b then
          (* this is a common case for simple objects *)
          let m = Shape.material (Intersection.shape a) in
          (Material.refractive_index m, 1.)
        else
          (* this is the case for two planes *)
          let m1 = Shape.material (Intersection.shape a) in
          let m2 = Shape.material (Intersection.shape b) in
          (Material.refractive_index m1, Material.refractive_index m2))
  | _ ->
      (* this is the slow case *)
      let rec loop iil containers =
        match iil with
        | [] -> (1., 1.)
        | hd :: tl ->
            let n1 =
              if hd = h then
                match containers with
                | [] -> 1.
                | a :: _ -> Material.refractive_index (Shape.material a)
              else 1. (* don't care, just need placeholder *)
            in

            let this_shape = Intersection.shape hd in
            let exists = List.exists (fun a -> a = this_shape) containers in
            let updated_containers =
              match exists with
              | true -> List.filter (fun a -> a <> this_shape) containers
              | false -> this_shape :: containers
            in

            if hd = h then
              let n2 =
                match updated_containers with
                | [] -> 1.
                | a :: _ -> Material.refractive_index (Shape.material a)
              in
              (n1, n2)
            else loop tl updated_containers
      in
      loop il []

let v i r il =
  let distance = Intersection.distance i in
  let shape = Intersection.shape i in
  let point = Ray.position r distance in
  let normalv = Intersection.normal_at shape point in
  let over_point =
    Tuple.add point (Tuple.multiply normalv (Float.epsilon *. 1000.))
  in
  let eyev = Tuple.negate (Ray.direction r) in
  let inside, normalv =
    if Tuple.dot normalv eyev < 0. then (true, Tuple.negate normalv)
    else (false, normalv)
  in
  let reflectv = Tuple.reflect (Ray.direction r) normalv in
  let n1, n2 = calc_n1_n2 i il in
  {
    distance;
    shape;
    point;
    over_point;
    eyev;
    normalv;
    reflectv;
    inside;
    n1;
    n2;
  }

let distance t = t.distance
let shape t = t.shape
let point t = t.point
let eyev t = t.eyev
let normalv t = t.normalv
let inside t = t.inside
let over_point t = t.over_point
let reflectv t = t.reflectv
let n_pair t = (t.n1, t.n2)
