open Raybook
open OUnit2

let test_create_precomputed _ =
  let r = Ray.v (Specialised.point 0. 0. (-5.)) (Specialised.vector 0. 0. 1.)
  and s = Shape.(v Sphere) in
  let i = Intersection.v s 4. in
  let res = Precomputed.v i r [ i ] in
  assert_equal 4. (Precomputed.distance res);
  assert_equal s (Precomputed.shape res);
  assert_bool "is equal"
    (Specialised.is_equal
       (Specialised.point 0. 0. (-1.))
       (Precomputed.point res));
  assert_bool "is equal"
    (Specialised.is_equal
       (Specialised.vector 0. 0. (-1.))
       (Precomputed.eyev res));
  assert_bool "is equal"
    (Specialised.is_equal
       (Specialised.vector 0. 0. (-1.))
       (Precomputed.normalv res))

let test_outside _ =
  let r = Ray.v (Specialised.point 0. 0. (-5.)) (Specialised.vector 0. 0. 1.)
  and s = Shape.(v Sphere) in
  let i = Intersection.v s 4. in
  let res = Precomputed.v i r [ i ] in
  assert_equal false (Precomputed.inside res)

let test_inside _ =
  let r = Ray.v (Specialised.point 0. 0. 0.) (Specialised.vector 0. 0. 1.)
  and s = Shape.(v Sphere) in
  let i = Intersection.v s 1. in
  let res = Precomputed.v i r [ i ] in
  assert_equal true (Precomputed.inside res);
  assert_bool "is equal"
    (Specialised.is_equal (Specialised.point 0. 0. 1.) (Precomputed.point res));
  assert_bool "is equal"
    (Specialised.is_equal
       (Specialised.vector 0. 0. (-1.))
       (Precomputed.eyev res));
  assert_bool "is equal"
    (Specialised.is_equal
       (Specialised.vector 0. 0. (-1.))
       (Precomputed.normalv res))

let suite =
  "Precomputed type tests"
  >::: [
         "Test create precomputed" >:: test_create_precomputed;
         "Test outside hit" >:: test_outside;
         "Test inside hit" >:: test_inside;
       ]

let () = run_test_tt_main suite
