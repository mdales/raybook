open Raybook
open OUnit2

let test_create_ray _ =
  let o = Specialised.point 1. 2. 3. and d = Specialised.vector 3. 4. 5. in
  let res = Ray.v o d in
  assert_equal o (Ray.origin res);
  assert_equal d (Ray.direction res)

let test_crete_ray_invalid_origin _ =
  let o = Specialised.vector 1. 2. 3. and d = Specialised.vector 3. 4. 5. in
  assert_raises (Invalid_argument "Origin must be a point") (fun () ->
      let _ = Ray.v o d in
      ())

let test_crete_ray_invalid_directon _ =
  let o = Specialised.point 1. 2. 3. and d = Specialised.point 3. 4. 5. in
  assert_raises (Invalid_argument "Direction must be a vector") (fun () ->
      let _ = Ray.v o d in
      ())

let test_position _ =
  let r = Ray.v (Specialised.point 2. 3. 4.) (Specialised.vector 1. 0. 0.) in
  let p1 = Ray.position r 0. in
  let e1 = Specialised.point 2. 3. 4. in
  assert_bool "is equal" (Specialised.is_equal e1 p1);
  let p2 = Ray.position r 1. in
  let e2 = Specialised.point 3. 3. 4. in
  assert_bool "is equal" (Specialised.is_equal e2 p2);
  let p3 = Ray.position r (-1.) in
  let e3 = Specialised.point 1. 3. 4. in
  assert_bool "is equal" (Specialised.is_equal e3 p3);
  let p4 = Ray.position r 2.5 in
  let e4 = Specialised.point 4.5 3. 4. in
  assert_bool "is equal" (Specialised.is_equal e4 p4)

let test_translate_ray _ =
  let r = Ray.v (Specialised.point 1. 2. 3.) (Specialised.vector 1. 0. 0.) in
  let t = Transformation.translation 3. 4. 5. in
  let res = Ray.transform r t in
  let expected_origin = Specialised.point 4. 6. 8.
  and expected_direction = Specialised.vector 1. 0. 0. in
  assert_bool "is equal" (Specialised.is_equal expected_origin (Ray.origin res));
  assert_bool "is equal"
    (Specialised.is_equal expected_direction (Ray.direction res))

let test_scaling_ray _ =
  let r = Ray.v (Specialised.point 1. 2. 3.) (Specialised.vector 0. 1. 0.) in
  let t = Transformation.scaling 2. 3. 4. in
  let res = Ray.transform r t in
  let expected_origin = Specialised.point 2. 6. 12.
  and expected_direction = Specialised.vector 0. 3. 0. in
  assert_bool "is equal" (Specialised.is_equal expected_origin (Ray.origin res));
  assert_bool "is equal"
    (Specialised.is_equal expected_direction (Ray.direction res))

let test_identity_on_ray_1 _ =
  let r = Ray.v (Specialised.point 0. 1. 0.) (Specialised.vector 0. (-1.) 0.) in
  let m = Specialised.identity () in
  let res = Ray.transform r m in
  assert_bool "is equal" (Specialised.is_equal (Ray.origin r) (Ray.origin res));
  assert_bool "is equal"
    (Specialised.is_equal (Ray.direction r) (Ray.direction res))

let test_identity_on_ray_2 _ =
  let r = Ray.v (Specialised.point 0. (-1.) 0.) (Specialised.vector 0. 1. 0.) in
  let m = Specialised.identity () in
  let res = Ray.transform r m in
  assert_bool "is equal" (Specialised.is_equal (Ray.origin r) (Ray.origin res));
  assert_bool "is equal"
    (Specialised.is_equal (Ray.direction r) (Ray.direction res))

let suite =
  "Ray tests"
  >::: [
         "Test create ray" >:: test_create_ray;
         "Test create ray with invalid origin" >:: test_crete_ray_invalid_origin;
         "Test create ray with invalid direction"
         >:: test_crete_ray_invalid_directon;
         "Test position" >:: test_position;
         "Test translate ray" >:: test_translate_ray;
         "Test scaling ray" >:: test_scaling_ray;
         "Test identity on ray 1" >:: test_identity_on_ray_1;
         "Test identity on ray 2" >:: test_identity_on_ray_2;
       ]

let () = run_test_tt_main suite
