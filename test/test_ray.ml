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

let suite =
  "Ray tests"
  >::: [
         "Test create ray" >:: test_create_ray;
         "Test create ray with invalid origin" >:: test_crete_ray_invalid_origin;
         "Test create ray with invalid direction"
         >:: test_crete_ray_invalid_directon;
       ]

let () = run_test_tt_main suite
