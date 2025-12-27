open Raybook
open OUnit2

let test_create_intersection _ =
  let s = Sphere.v 42 in
  let res = Intersection.v (Intersection.Sphere s) 3.5 in
  assert_equal 3.5 (Intersection.distance res);
  assert_equal (Intersection.Sphere s) (Intersection.shape res)

let suite =
  "Intersection tests"
  >::: [ "Test create intersection" >:: test_create_intersection ]

let () = run_test_tt_main suite
