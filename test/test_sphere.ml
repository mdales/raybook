open Raybook
open OUnit2

let almost_equal a b =
  assert_bool
    (Printf.sprintf "%f vs %f" a b)
    (Float.abs (a -. b) < Float.epsilon)

let test_intersect_at_two_points _ =
  let r = Ray.v (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.) in
  let s = Sphere.v 42 in
  let xs = Intersection.intersects (Intersection.Sphere s) r in
  match xs with
  | [ t1; t2 ] ->
      almost_equal 4. (Intersection.distance t1);
      almost_equal 6. (Intersection.distance t2)
  | _ -> assert_bool "no intersects" false

let test_intersect_at_tangent _ =
  let r = Ray.v (Tuple.point 0. 1. (-5.)) (Tuple.vector 0. 0. 1.) in
  let s = Sphere.v 42 in
  let xs = Intersection.intersects (Intersection.Sphere s) r in
  match xs with
  | [ t1; t2 ] ->
      almost_equal 5. (Intersection.distance t1);
      almost_equal 5. (Intersection.distance t2)
  | _ -> assert_bool "no intersects" false

let test_no_intersect _ =
  let r = Ray.v (Tuple.point 0. 2. (-5.)) (Tuple.vector 0. 0. 1.) in
  let s = Sphere.v 42 in
  let xs = Intersection.intersects (Intersection.Sphere s) r in
  match xs with [] -> () | _ -> assert_bool "expected no answer" false

let test_ray_inside_sphere _ =
  let r = Ray.v (Tuple.point 0. 0. 0.) (Tuple.vector 0. 0. 1.) in
  let s = Sphere.v 42 in
  let xs = Intersection.intersects (Intersection.Sphere s) r in
  match xs with
  | [ t1; t2 ] ->
      almost_equal (-1.) (Intersection.distance t1);
      almost_equal 1. (Intersection.distance t2)
  | _ -> assert_bool "no intersects" false

let test_ray_behind_sphere _ =
  let r = Ray.v (Tuple.point 0. 0. 5.) (Tuple.vector 0. 0. 1.) in
  let s = Sphere.v 42 in
  let xs = Intersection.intersects (Intersection.Sphere s) r in
  match xs with
  | [ t1; t2 ] ->
      almost_equal (-6.) (Intersection.distance t1);
      almost_equal (-4.) (Intersection.distance t2)
  | _ -> assert_bool "no intersects" false

let suite =
  "Sphere tests"
  >::: [
         "Test intersect at two points" >:: test_intersect_at_two_points;
         "Test intersect at tangent" >:: test_intersect_at_tangent;
         "Test no intersect" >:: test_no_intersect;
         "Test ray inside spehere" >:: test_ray_inside_sphere;
         "Test ray behind spehere" >:: test_ray_behind_sphere;
       ]

let () = run_test_tt_main suite
