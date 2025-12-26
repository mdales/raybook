open Raybook
open OUnit2

let test_create_ray _ =
  let o = Tuple.point 1. 2. 3. and d = Tuple.vector 3. 4. 5. in
  let res = Ray.v o d in
  assert_equal o (Ray.origin res);
  assert_equal d (Ray.direction res)

let test_crete_ray_invalid_origin _ =
  let o = Tuple.vector 1. 2. 3. and d = Tuple.vector 3. 4. 5. in
  assert_raises (Invalid_argument "Origin must be a point") (fun () ->
      let _ = Ray.v o d in
      ())

let test_crete_ray_invalid_directon _ =
  let o = Tuple.point 1. 2. 3. and d = Tuple.point 3. 4. 5. in
  assert_raises (Invalid_argument "Direction must be a vector") (fun () ->
      let _ = Ray.v o d in
      ())

let test_position _ =
  let r = Ray.v (Tuple.point 2. 3. 4.) (Tuple.vector 1. 0. 0.) in
  let p1 = Ray.position r 0. in
  let e1 = Tuple.point 2. 3. 4. in
  assert_bool "is equal" (Tuple.is_equal e1 p1);
  let p2 = Ray.position r 1. in
  let e2 = Tuple.point 3. 3. 4. in
  assert_bool "is equal" (Tuple.is_equal e2 p2);
  let p3 = Ray.position r (-1.) in
  let e3 = Tuple.point 1. 3. 4. in
  assert_bool "is equal" (Tuple.is_equal e3 p3);
  let p4 = Ray.position r 2.5 in
  let e4 = Tuple.point 4.5 3. 4. in
  assert_bool "is equal" (Tuple.is_equal e4 p4)

let suite =
  "Ray tests"
  >::: [
         "Test create ray" >:: test_create_ray;
         "Test create ray with invalid origin" >:: test_crete_ray_invalid_origin;
         "Test create ray with invalid direction"
         >:: test_crete_ray_invalid_directon;
         "Test position" >:: test_position;
       ]

let () = run_test_tt_main suite
