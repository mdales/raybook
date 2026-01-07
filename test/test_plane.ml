open Raybook
open OUnit2

let almost_equal a b =
  assert_bool
    (Printf.sprintf "%f vs %f" a b)
    (Float.abs (a -. b) < Float.epsilon)

let test_create_plane_defaults _ =
  let s = Shape.(v Plane) in
  assert_equal Shape.Plane (Shape.geometry s);
  let expected_colour = Colour.v 1. 1. 1. in
  let expected_material =
    Material.v ~pattern:Pattern.(v (Solid expected_colour)) ()
  in
  assert_equal expected_material (Shape.material s);
  let ident = Matrix.identity 4 in
  assert_bool "is equal" (Matrix.is_equal ident (Shape.transform s));
  assert_bool "is equal" (Matrix.is_equal ident (Shape.inverse_transform s));
  assert_bool "is equal"
    (Matrix.is_equal ident (Shape.transpose_inverse_transform s))

let test_plane_normal _ =
  let s = Shape.v Shape.Plane in
  let samples =
    [
      Tuple.point 0. 0. 0.; Tuple.point 10. 0. (-10.); Tuple.point (-5.) 0. 150.;
    ]
  in
  List.iter
    (fun p ->
      let res = Intersection.normal_at s p in
      let expected = Tuple.vector 0. 1. 0. in
      assert_bool "is equal" (Tuple.is_equal expected res))
    samples

let test_plane_vs_parallel_ray _ =
  let s = Shape.v Shape.Plane in
  let r = Ray.v (Tuple.point 0. 10. 0.) (Tuple.vector 0. 0. 1.) in
  let res = Intersection.intersects s r in
  assert_equal 0 (List.length res)

let test_plane_vs_coplanar_ray _ =
  let s = Shape.v Shape.Plane in
  let r = Ray.v (Tuple.point 0. 0. 0.) (Tuple.vector 0. 0. 1.) in
  let res = Intersection.intersects s r in
  assert_equal 0 (List.length res)

let test_intersect_plane_from_above _ =
  let s = Shape.v Shape.Plane in
  let r = Ray.v (Tuple.point 0. 1. 0.) (Tuple.vector 0. (-1.) 0.) in
  let res = Intersection.intersects s r in
  assert_equal 1 (List.length res);
  let hit = List.hd res in
  almost_equal 1. (Intersection.distance hit);
  assert_equal s (Intersection.shape hit)

let test_intersect_plane_from_below _ =
  let s = Shape.v Shape.Plane in
  let r = Ray.v (Tuple.point 0. (-1.) 0.) (Tuple.vector 0. 1. 0.) in
  let res = Intersection.intersects s r in
  assert_equal 1 (List.length res);
  let hit = List.hd res in
  almost_equal 1. (Intersection.distance hit);
  assert_equal s (Intersection.shape hit)

let suite =
  "Plane tests"
  >::: [
         "Test create sphere with defaults" >:: test_create_plane_defaults;
         "Test plane normal is same everywhere" >:: test_plane_normal;
         "Test plane does not intersect with parallel ray"
         >:: test_plane_vs_parallel_ray;
         "Test plane with ray inside it does not intersect"
         >:: test_plane_vs_coplanar_ray;
         "Test plane intersection from above"
         >:: test_intersect_plane_from_above;
         "Test plane intersection from below"
         >:: test_intersect_plane_from_below;
       ]

let () = run_test_tt_main suite
