open Raybook
open OUnit2

let almost_equal a b =
  assert_bool
    (Printf.sprintf "%f vs %f" a b)
    (Float.abs (a -. b) < Float.epsilon)

let test_create_sphere_default_material _ =
  let s = Sphere.v 42 in
  assert_equal 42 (Sphere.id s);
  let expected_colour = Colour.v 1. 1. 1. in
  let expected_material = Material.v ~colour:expected_colour () in
  assert_equal expected_material (Sphere.material s)

let test_create_sphere_with_material _ =
  let expected_colour = Colour.v 0.1 0.2 0.3 in
  let material = Material.v ~colour:expected_colour ~ambient:0.4 () in
  let s = Sphere.v ~material 42 in
  assert_equal 42 (Sphere.id s);
  let rm = Sphere.material s in
  assert_equal rm (Sphere.material s)

let test_intersect_at_two_points _ =
  let r = Ray.v (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.) in
  let s = Sphere.v 42 in
  let xs = Intersection.intersects (Shape.Sphere s) r in
  match xs with
  | [ t1; t2 ] ->
      almost_equal 4. (Intersection.distance t1);
      almost_equal 6. (Intersection.distance t2)
  | _ -> assert_bool "no intersects" false

let test_intersect_at_tangent _ =
  let r = Ray.v (Tuple.point 0. 1. (-5.)) (Tuple.vector 0. 0. 1.) in
  let s = Sphere.v 42 in
  let xs = Intersection.intersects (Shape.Sphere s) r in
  match xs with
  | [ t1; t2 ] ->
      almost_equal 5. (Intersection.distance t1);
      almost_equal 5. (Intersection.distance t2)
  | _ -> assert_bool "no intersects" false

let test_no_intersect _ =
  let r = Ray.v (Tuple.point 0. 2. (-5.)) (Tuple.vector 0. 0. 1.) in
  let s = Sphere.v 42 in
  let xs = Intersection.intersects (Shape.Sphere s) r in
  match xs with [] -> () | _ -> assert_bool "expected no answer" false

let test_ray_inside_sphere _ =
  let r = Ray.v (Tuple.point 0. 0. 0.) (Tuple.vector 0. 0. 1.) in
  let s = Sphere.v 42 in
  let xs = Intersection.intersects (Shape.Sphere s) r in
  match xs with
  | [ t1; t2 ] ->
      almost_equal (-1.) (Intersection.distance t1);
      almost_equal 1. (Intersection.distance t2)
  | _ -> assert_bool "no intersects" false

let test_ray_behind_sphere _ =
  let r = Ray.v (Tuple.point 0. 0. 5.) (Tuple.vector 0. 0. 1.) in
  let s = Sphere.v 42 in
  let xs = Intersection.intersects (Shape.Sphere s) r in
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
  let xs = Intersection.intersects (Shape.Sphere s) r in
  match xs with
  | [ t1; t2 ] ->
      almost_equal 3. (Intersection.distance t1);
      almost_equal 7. (Intersection.distance t2)
  | _ -> assert_bool "no intersects" false

let test_translated_sphere_intersection _ =
  let r = Ray.v (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.) in
  let t = Transformation.translation 5. 0. 0. in
  let s = Sphere.set_transform (Sphere.v 42) t in
  let xs = Intersection.intersects (Shape.Sphere s) r in
  match xs with [] -> () | _ -> assert_bool "expected no answer" false

let test_normal_at_x_axis _ =
  let s = Sphere.v 42 in
  let p = Tuple.point 1. 0. 0. in
  let res = Intersection.normal_at (Shape.Sphere s) p in
  let expected = Tuple.vector 1. 0. 0. in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_normal_at_y_axis _ =
  let s = Sphere.v 42 in
  let p = Tuple.point 0. 1. 0. in
  let res = Intersection.normal_at (Shape.Sphere s) p in
  let expected = Tuple.vector 0. 1. 0. in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_normal_at_z_axis _ =
  let s = Sphere.v 42 in
  let p = Tuple.point 0. 0. 1. in
  let res = Intersection.normal_at (Shape.Sphere s) p in
  let expected = Tuple.vector 0. 0. 1. in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_normal_at_off_axis _ =
  let v = Float.sqrt 3. /. 3. in
  let s = Sphere.v 42 in
  let p = Tuple.point v v v in
  let res = Intersection.normal_at (Shape.Sphere s) p in
  let expected = Tuple.vector v v v in
  assert_bool "is equal" (Tuple.is_equal expected res);
  let normal_res = Tuple.normalize res in
  assert_bool "is equal" (Tuple.is_equal res normal_res)

let test_normal_at_on_translated_sphere _ =
  let t = Transformation.translation 0. 1. 0. in
  let s = Sphere.set_transform (Sphere.v 42) t in
  let v = Float.sqrt 2. /. 2. in
  let p = Tuple.point 0. (1. +. v) (0. -. v) in
  let res = Intersection.normal_at (Shape.Sphere s) p in
  let expected = Tuple.vector 0. v (-1. *. v) in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_normal_at_on_tranformed_sphere _ =
  let st = Transformation.scaling 1. 0.5 1.
  and rt = Transformation.rotate_z (Float.pi /. 5.) in
  let t = Matrix.multiply st rt in
  let s = Sphere.set_transform (Sphere.v 42) t in
  let v = Float.sqrt 2. /. 2. in
  let p = Tuple.point 0. v (-1. *. v) in
  let res = Intersection.normal_at (Shape.Sphere s) p in
  let expected =
    Tuple.vector 0. (4. /. Float.sqrt 17.) (-1. /. Float.sqrt 17.)
  in
  assert_bool "is equal" (Tuple.is_equal expected res)

let suite =
  "Sphere tests"
  >::: [
         "Test create sphere with defaults"
         >:: test_create_sphere_default_material;
         "Test create sphere with material" >:: test_create_sphere_with_material;
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
         "Test normal at on translated sphere"
         >:: test_normal_at_on_translated_sphere;
         "Test normal at on transformed spehere"
         >:: test_normal_at_on_tranformed_sphere;
       ]

let () = run_test_tt_main suite
