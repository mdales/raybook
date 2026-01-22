open Raybook
open OUnit2

let test_sphere_bounds _ =
  (* Transform should be ignored *)
  let t = Transformation.translation 1. 2. 3. in
  let s = Shape.(v ~transform:t Sphere) in
  let minb, maxb = Shape.bounds s in
  let expected_min = Specialised.point (-1.) (-1.) (-1.)
  and expected_max = Specialised.point 1. 1. 1. in
  assert_bool "is equal" (Specialised.is_equal expected_min minb);
  assert_bool "is equal" (Specialised.is_equal expected_max maxb)

let test_cube_bounds _ =
  (* Transform should be ignored *)
  let t = Transformation.translation 1. 2. 3. in
  let s = Shape.(v ~transform:t Cube) in
  let minb, maxb = Shape.bounds s in
  let expected_min = Specialised.point (-1.) (-1.) (-1.)
  and expected_max = Specialised.point 1. 1. 1. in
  assert_bool "is equal" (Specialised.is_equal expected_min minb);
  assert_bool "is equal" (Specialised.is_equal expected_max maxb)

let test_cylinder_bounds _ =
  (* Transform should be ignored *)
  let t = Transformation.translation 1. 2. 3. in
  let s =
    Shape.(v ~transform:t (Cylinder { min = -3.; max = 4.; capped = false }))
  in
  let minb, maxb = Shape.bounds s in
  let expected_min = Specialised.point (-1.) (-3.) (-1.)
  and expected_max = Specialised.point 1. 4. 1. in
  assert_bool "is equal" (Specialised.is_equal expected_min minb);
  assert_bool "is equal" (Specialised.is_equal expected_max maxb)

let test_plane_bounds _ =
  (* Transform should be ignored *)
  let t = Transformation.translation 1. 2. 3. in
  let s = Shape.(v ~transform:t Plane) in
  let minb, maxb = Shape.bounds s in
  let expected_min = Specialised.point Float.neg_infinity 0. Float.neg_infinity
  and expected_max = Specialised.point Float.infinity 0. Float.infinity in
  assert_equal (Specialised.x expected_min) (Specialised.x minb);
  assert_equal (Specialised.y expected_min) (Specialised.y minb);
  assert_equal (Specialised.z expected_min) (Specialised.z minb);
  assert_equal (Specialised.x expected_max) (Specialised.x maxb);
  assert_equal (Specialised.y expected_max) (Specialised.y maxb);
  assert_equal (Specialised.z expected_max) (Specialised.z maxb)

let test_cone_bounds _ =
  (* Transform should be ignored *)
  let t = Transformation.translation 1. 2. 3. in
  let s =
    Shape.(v ~transform:t (Cone { min = -3.; max = 4.; capped = false }))
  in
  let minb, maxb = Shape.bounds s in
  let expected_min = Specialised.point (-4.) (-3.) (-4.)
  and expected_max = Specialised.point 4. 4. 4. in
  assert_bool "is equal" (Specialised.is_equal expected_min minb);
  assert_bool "is equal" (Specialised.is_equal expected_max maxb)

let test_group_bounds_with_untransformed_object _ =
  let inner = Shape.(v Sphere) in
  let s = Shape.(v (Group [ inner ])) in
  let minb, maxb = Shape.bounds s in
  let expected_min = Specialised.point (-1.) (-1.) (-1.)
  and expected_max = Specialised.point 1. 1. 1. in
  assert_bool "is equal" (Specialised.is_equal expected_min minb);
  assert_bool "is equal" (Specialised.is_equal expected_max maxb)

let test_group_bounds_with_transformed_object _ =
  let innert = Transformation.scaling 2. 3. 4. in
  let inner = Shape.(v ~transform:innert Sphere) in
  let s = Shape.(v (Group [ inner ])) in
  let minb, maxb = Shape.bounds s in
  let expected_min = Specialised.point (-2.) (-3.) (-4.)
  and expected_max = Specialised.point 2. 3. 4. in
  assert_bool "is equal" (Specialised.is_equal expected_min minb);
  assert_bool "is equal" (Specialised.is_equal expected_max maxb)

let test_transformed_group_bounds_with_untransformed_object _ =
  let t = Transformation.translation 1. 2. 3. in
  let inner = Shape.(v Sphere) in
  let s = Shape.(v ~transform:t (Group [ inner ])) in
  let minb, maxb = Shape.bounds s in
  let expected_min = Specialised.point 0. 1. 2.
  and expected_max = Specialised.point 2. 3. 4. in
  assert_bool "is equal" (Specialised.is_equal expected_min minb);
  assert_bool "is equal" (Specialised.is_equal expected_max maxb)

let test_transformed_group_bounds_with_transformed_object _ =
  let t = Transformation.translation 1. 2. 3. in
  let innert = Transformation.scaling 2. 3. 4. in
  let inner = Shape.(v ~transform:innert Sphere) in
  let s = Shape.(v ~transform:t (Group [ inner ])) in
  let minb, maxb = Shape.bounds s in
  let expected_min = Specialised.point (-1.) (-1.) (-1.)
  and expected_max = Specialised.point 3. 5. 7. in
  assert_bool "is equal" (Specialised.is_equal expected_min minb);
  assert_bool "is equal" (Specialised.is_equal expected_max maxb)

let test_group_with_multiple_objects _ =
  let t1 = Transformation.translation (-4.) 0. 0. in
  let s1 = Shape.(v ~transform:t1 Sphere) in
  let t2 = Transformation.translation 4. 0. 0. in
  let s2 = Shape.(v ~transform:t2 Sphere) in
  let s = Shape.(v (Group [ s1; s2 ])) in
  let minb, maxb = Shape.bounds s in
  let expected_min = Specialised.point (-5.) (-1.) (-1.)
  and expected_max = Specialised.point 5. 1. 1. in
  assert_bool "is equal" (Specialised.is_equal expected_min minb);
  assert_bool "is equal" (Specialised.is_equal expected_max maxb)

let suite =
  "Bounds tests"
  >::: [
         "Test sphere bounds" >:: test_sphere_bounds;
         "Test cube bounds" >:: test_cube_bounds;
         "Test cylinder bounds" >:: test_cylinder_bounds;
         "Test plane bounds" >:: test_plane_bounds;
         "Test cone bounds" >:: test_cone_bounds;
         "Test untrasnformed group with untransformed object"
         >:: test_group_bounds_with_untransformed_object;
         "Test untrassformed group with transformed object"
         >:: test_group_bounds_with_transformed_object;
         "Test transformed group with untransformed object"
         >:: test_transformed_group_bounds_with_untransformed_object;
         "Test transformed group with transformed object"
         >:: test_transformed_group_bounds_with_transformed_object;
         "Test group with multiple objects" >:: test_group_with_multiple_objects;
       ]

let () = run_test_tt_main suite
