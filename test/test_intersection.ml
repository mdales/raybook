open Raybook
open OUnit2

let test_create_intersection _ =
  let s = Sphere.v 42 in
  let res = Intersection.v (Intersection.Sphere s) 3.5 in
  assert_equal 3.5 (Intersection.distance res);
  assert_equal (Intersection.Sphere s) (Intersection.shape res)

let test_hit_all_positive _ =
  let s = Intersection.Sphere (Sphere.v 42) in
  let tl = [ Intersection.v s 1.; Intersection.v s 2. ] in
  let res = Intersection.hit tl in
  match res with
  | None -> assert_bool "unexpected" false
  | Some i -> assert_equal 1. (Intersection.distance i)

let test_hit_mixed_positive_negative _ =
  let s = Intersection.Sphere (Sphere.v 42) in
  let tl = [ Intersection.v s (-1.); Intersection.v s 1. ] in
  let res = Intersection.hit tl in
  match res with
  | None -> assert_bool "unexpected" false
  | Some i -> assert_equal 1. (Intersection.distance i)

let test_hit_all_negative _ =
  let s = Intersection.Sphere (Sphere.v 42) in
  let tl = [ Intersection.v s (-2.); Intersection.v s (-1.) ] in
  let res = Intersection.hit tl in
  match res with None -> () | Some _ -> assert_bool "unexpected" false

let test_mixed_order_mixed_sign _ =
  let s = Intersection.Sphere (Sphere.v 42) in
  let tl =
    [
      Intersection.v s 5.;
      Intersection.v s 7.;
      Intersection.v s (-3.);
      Intersection.v s 2.;
    ]
  in
  let res = Intersection.hit tl in
  match res with
  | None -> assert_bool "unexpected" false
  | Some i -> assert_equal 2. (Intersection.distance i)

let suite =
  "Intersection tests"
  >::: [
         "Test create intersection" >:: test_create_intersection;
         "Test hit all positive" >:: test_hit_all_positive;
         "Test hit mixed" >:: test_hit_mixed_positive_negative;
         "Test hit all negative" >:: test_hit_all_negative;
         "Test mixed order" >:: test_mixed_order_mixed_sign;
       ]

let () = run_test_tt_main suite
