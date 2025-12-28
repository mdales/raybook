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

let test_default_transform _ =
  let s = Sphere.v 42 in
  let res = Sphere.transform s in
  let expected = Matrix.identity 4 in
  assert_bool "is equal" (Matrix.is_equal expected res)

let test_set_transform _ =
  let s = Sphere.v 42 in
  let t = Transformation.translation 2. 3. 4. in
  let updated = Sphere.set_transform s t in
  let res = Sphere.transform updated in
  assert_bool "is equal" (Matrix.is_equal t res)

let test_scaled_sphere_intersection _ =
  let r = Ray.v (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.) in
  let t = Transformation.scaling 2. 2. 2. in
  let s = Sphere.set_transform (Sphere.v 42) t in
  let xs = Intersection.intersects (Intersection.Sphere s) r in
  match xs with
  | [ t1; t2 ] ->
      almost_equal 3. (Intersection.distance t1);
      almost_equal 7. (Intersection.distance t2)
  | _ -> assert_bool "no intersects" false

let test_translated_sphere_intersection _ =
  let r = Ray.v (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.) in
  let t = Transformation.translation 5. 0. 0. in
  let s = Sphere.set_transform (Sphere.v 42) t in
  let xs = Intersection.intersects (Intersection.Sphere s) r in
  match xs with [] -> () | _ -> assert_bool "expected no answer" false

let test_normal_at_x_axis _ =
  let s = Sphere.v 42 in
  let p = Tuple.point 1. 0. 0. in
  let res = Intersection.normal_at (Intersection.Sphere s) p in
  let expected = Tuple.vector 1. 0. 0. in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_normal_at_y_axis _ =
  let s = Sphere.v 42 in
  let p = Tuple.point 0. 1. 0. in
  let res = Intersection.normal_at (Intersection.Sphere s) p in
  let expected = Tuple.vector 0. 1. 0. in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_normal_at_z_axis _ =
  let s = Sphere.v 42 in
  let p = Tuple.point 0. 0. 1. in
  let res = Intersection.normal_at (Intersection.Sphere s) p in
  let expected = Tuple.vector 0. 0. 1. in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_normal_at_off_axis _ =
  let v = Float.sqrt 3. /. 3. in
  let s = Sphere.v 42 in
  let p = Tuple.point v v v in
  let res = Intersection.normal_at (Intersection.Sphere s) p in
  let expected = Tuple.vector v v v in
  assert_bool "is equal" (Tuple.is_equal expected res);
  let normal_res = Tuple.normalize res in
  assert_bool "is equal" (Tuple.is_equal res normal_res)

let suite =
  "Sphere tests"
  >::: [
         "Test intersect at two points" >:: test_intersect_at_two_points;
         "Test intersect at tangent" >:: test_intersect_at_tangent;
         "Test no intersect" >:: test_no_intersect;
         "Test ray inside spehere" >:: test_ray_inside_sphere;
         "Test ray behind spehere" >:: test_ray_behind_sphere;
         "Test default transform" >:: test_default_transform;
         "Test set transform" >:: test_set_transform;
         "Test scaled intersection" >:: test_scaled_sphere_intersection;
         "Test translated intersection" >:: test_translated_sphere_intersection;
         "Test normal at on x axis" >:: test_normal_at_x_axis;
         "Test normal at on y axis" >:: test_normal_at_y_axis;
         "Test normal at on z axis" >:: test_normal_at_z_axis;
         "Test normal at off axis" >:: test_normal_at_off_axis;
       ]

let () = run_test_tt_main suite
