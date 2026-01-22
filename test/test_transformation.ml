open Raybook
open OUnit2

let test_point_translation _ =
  let t = Transformation.translation 5. (-3.) 2. in
  let p = Specialised.point (-3.) 4. 5. in
  let res = Specialised.multiply t p in
  let expected = Specialised.point 2. 1. 7. in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_point_inverse_translation _ =
  let t = Transformation.translation 5. (-3.) 2. in
  let it = Specialised.inverse t in
  let p = Specialised.point (-3.) 4. 5. in
  let res = Specialised.multiply it p in
  let expected = Specialised.point (-8.) 7. 3. in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_vector_translation _ =
  let t = Transformation.translation 5. (-3.) 2. in
  let v = Specialised.vector (-3.) 4. 5. in
  let res = Specialised.multiply t v in
  assert_bool "is equal" (Specialised.is_equal v res)

let test_point_scaling _ =
  let t = Transformation.scaling 2. 3. 4. in
  let p = Specialised.point (-4.) 6. 8. in
  let res = Specialised.multiply t p in
  let expected = Specialised.point (-8.) 18. 32. in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_vector_scaling _ =
  let t = Transformation.scaling 2. 3. 4. in
  let v = Specialised.vector (-4.) 6. 8. in
  let res = Specialised.multiply t v in
  let expected = Specialised.vector (-8.) 18. 32. in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_vector_inverse_scaling _ =
  let t = Transformation.scaling 2. 3. 4. in
  let it = Specialised.inverse t in
  let v = Specialised.vector (-4.) 6. 8. in
  let res = Specialised.multiply it v in
  let expected = Specialised.vector (-2.) 2. 2. in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_point_reflection _ =
  let t = Transformation.scaling (-1.) 1. 1. in
  let p = Specialised.point 2. 3. 4. in
  let res = Specialised.multiply t p in
  let expected = Specialised.point (-2.) 3. 4. in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_rotate_point_on_x _ =
  let p = Specialised.point 0. 1. 0. in
  let half_quarter = Transformation.rotate_x (Float.pi /. 4.)
  and full_quarter = Transformation.rotate_x (Float.pi /. 2.) in
  let half_res = Specialised.multiply half_quarter p
  and full_res = Specialised.multiply full_quarter p in
  let half_expected =
    Specialised.point 0. (Float.sqrt 2. /. 2.) (Float.sqrt 2. /. 2.)
  and full_expected = Specialised.point 0. 0. 1. in
  assert_bool "is equal" (Specialised.is_equal half_expected half_res);
  assert_bool "is equal" (Specialised.is_equal full_expected full_res)

let test_inverse_rotate_point_on_x _ =
  let p = Specialised.point 0. 1. 0. in
  let half_quarter = Transformation.rotate_x (Float.pi /. 4.) in
  let inv_half_quarter = Specialised.inverse half_quarter in
  let half_res = Specialised.multiply inv_half_quarter p in
  let half_expected =
    Specialised.point 0. (Float.sqrt 2. /. 2.) (Float.sqrt 2. /. -2.)
  in
  assert_bool "is equal" (Specialised.is_equal half_expected half_res)

let test_rotate_point_on_y _ =
  let p = Specialised.point 0. 0. 1. in
  let half_quarter = Transformation.rotate_y (Float.pi /. 4.)
  and full_quarter = Transformation.rotate_y (Float.pi /. 2.) in
  let half_res = Specialised.multiply half_quarter p
  and full_res = Specialised.multiply full_quarter p in
  let half_expected =
    Specialised.point (Float.sqrt 2. /. 2.) 0. (Float.sqrt 2. /. 2.)
  and full_expected = Specialised.point 1. 0. 0. in
  assert_bool "is equal" (Specialised.is_equal half_expected half_res);
  assert_bool "is equal" (Specialised.is_equal full_expected full_res)

let test_rotate_point_on_z _ =
  let p = Specialised.point 0. 1. 0. in
  let half_quarter = Transformation.rotate_z (Float.pi /. 4.)
  and full_quarter = Transformation.rotate_z (Float.pi /. 2.) in
  let half_res = Specialised.multiply half_quarter p
  and full_res = Specialised.multiply full_quarter p in
  let half_expected =
    Specialised.point (Float.sqrt 2. /. -2.) (Float.sqrt 2. /. 2.) 0.
  and full_expected = Specialised.point (-1.) 0. 0. in
  assert_bool "is equal" (Specialised.is_equal half_expected half_res);
  assert_bool "is equal" (Specialised.is_equal full_expected full_res)

let test_shearing_x_by_y _ =
  let t = Transformation.shearing 1. 0. 0. 0. 0. 0. in
  let p = Specialised.point 2. 3. 4. in
  let res = Specialised.multiply t p in
  let expected = Specialised.point 5. 3. 4. in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_shearing_x_by_z _ =
  let t = Transformation.shearing 0. 1. 0. 0. 0. 0. in
  let p = Specialised.point 2. 3. 4. in
  let res = Specialised.multiply t p in
  let expected = Specialised.point 6. 3. 4. in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_shearing_y_by_x _ =
  let t = Transformation.shearing 0. 0. 1. 0. 0. 0. in
  let p = Specialised.point 2. 3. 4. in
  let res = Specialised.multiply t p in
  let expected = Specialised.point 2. 5. 4. in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_shearing_y_by_z _ =
  let t = Transformation.shearing 0. 0. 0. 1. 0. 0. in
  let p = Specialised.point 2. 3. 4. in
  let res = Specialised.multiply t p in
  let expected = Specialised.point 2. 7. 4. in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_shearing_z_by_x _ =
  let t = Transformation.shearing 0. 0. 0. 0. 1. 0. in
  let p = Specialised.point 2. 3. 4. in
  let res = Specialised.multiply t p in
  let expected = Specialised.point 2. 3. 6. in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_shearing_z_by_y _ =
  let t = Transformation.shearing 0. 0. 0. 0. 0. 1. in
  let p = Specialised.point 2. 3. 4. in
  let res = Specialised.multiply t p in
  let expected = Specialised.point 2. 3. 7. in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_sequence_transformations_1 _ =
  let p = Specialised.point 1. 0. 1. in
  let a = Transformation.rotate_x (Float.pi /. 2.)
  and b = Transformation.scaling 5. 5. 5.
  and c = Transformation.translation 10. 5. 7. in
  let p2 = Specialised.multiply a p in
  let expected2 = Specialised.point 1. (-1.) 0. in
  assert_bool "is equal 1" (Specialised.is_equal expected2 p2);
  let p3 = Specialised.multiply b p2 in
  let expected3 = Specialised.point 5. (-5.) 0. in
  assert_bool "is equal 2" (Specialised.is_equal expected3 p3);
  let p4 = Specialised.multiply c p3 in
  let expected4 = Specialised.point 15. 0. 7. in
  assert_bool "is equal 3" (Specialised.is_equal expected4 p4)

let test_sequence_transformations_2 _ =
  let p = Specialised.point 1. 0. 1. in
  let a = Transformation.rotate_x (Float.pi /. 2.)
  and b = Transformation.scaling 5. 5. 5.
  and c = Transformation.translation 10. 5. 7. in
  let t = Specialised.multiply (Specialised.multiply c b) a in
  let res = Specialised.multiply t p in
  let expected = Specialised.point 15. 0. 7. in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_default_view_transform _ =
  let from_p = Specialised.point 0. 0. 0.
  and to_p = Specialised.point 0. 0. (-1.)
  and up = Specialised.vector 0. 1. 0. in
  let res = Transformation.view_transform from_p to_p up in
  assert_bool "is equal" (Specialised.is_equal (Specialised.identity ()) res)

let test_towards_z_view_transform _ =
  let from_p = Specialised.point 0. 0. 0.
  and to_p = Specialised.point 0. 0. 1.
  and up = Specialised.vector 0. 1. 0. in
  let res = Transformation.view_transform from_p to_p up in
  assert_bool "is equal"
    (Specialised.is_equal (Transformation.scaling (-1.) 1. (-1.)) res)

let test_arbitrary_view_transform _ =
  let from_p = Specialised.point 1. 3. 2.
  and to_p = Specialised.point 4. (-2.) 8.
  and up = Specialised.vector 1. 1. 0. in
  let res = Transformation.view_transform from_p to_p up in
  let expected =
    Specialised.of_array
      [|
        [|
          -0.50709255283710985562;
          0.50709255283710985562;
          0.67612340378281310382;
          -2.36643191323984591889;
        |];
        [|
          0.76771593385967995538;
          0.60609152673132626887;
          0.12121830534626525100;
          -2.82842712474618895868;
        |];
        [|
          -0.35856858280031805863;
          0.59761430466719678289;
          -0.71713716560063611727;
          -0.00000000000000005551;
        |];
        [|
          0.00000000000000000000;
          0.00000000000000000000;
          0.00000000000000000000;
          1.00000000000000000000;
        |];
      |]
  in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_combine_empty _ =
  let res = Transformation.combine [] in
  let expected = Specialised.identity () in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_combine_single_transform _ =
  let t = Transformation.translation 1. 2. 3. in
  let res = Transformation.combine [ t ] in
  assert_bool "is equal" (Specialised.is_equal t res)

let test_combine_multi_transform _ =
  let t1 = Transformation.translation 1. 2. 3.
  and t2 = Transformation.scaling 3. 4. 5.
  and t3 = Transformation.rotate_x 2. in
  let res = Transformation.combine [ t1; t2; t3 ] in
  let expected = Specialised.multiply (Specialised.multiply t3 t2) t1 in
  assert_bool "is equal" (Specialised.is_equal expected res)

let suite =
  "Transformation tests"
  >::: [
         "Test point translation" >:: test_point_translation;
         "Test point translation inverse" >:: test_point_inverse_translation;
         "Test vector translation" >:: test_vector_translation;
         "Test point scaling" >:: test_point_scaling;
         "Test vector scaling" >:: test_vector_scaling;
         "Test vector translation inverse" >:: test_vector_inverse_scaling;
         "Test point reflection" >:: test_point_reflection;
         "Test rotate point on x" >:: test_rotate_point_on_x;
         "Test inverse rotate point on x" >:: test_inverse_rotate_point_on_x;
         "Test rotate point on y" >:: test_rotate_point_on_y;
         "Test rotate point on z" >:: test_rotate_point_on_z;
         "Test shearing x by y" >:: test_shearing_x_by_y;
         "Test shearing x by z" >:: test_shearing_x_by_z;
         "Test shearing y by x" >:: test_shearing_y_by_x;
         "Test shearing y by z" >:: test_shearing_y_by_z;
         "Test shearing z by x" >:: test_shearing_z_by_x;
         "Test shearing z by y" >:: test_shearing_z_by_y;
         "Test sequence of transformations 1"
         >:: test_sequence_transformations_1;
         "Test sequence of transformations 2"
         >:: test_sequence_transformations_2;
         "Test default view transformation" >:: test_default_view_transform;
         "Test view towards z transformation" >:: test_towards_z_view_transform;
         "Test arbitrary view transformation" >:: test_arbitrary_view_transform;
         "Test combine empty list" >:: test_combine_empty;
         "Test combine single transform" >:: test_combine_single_transform;
         "Test combine multiple transforms" >:: test_combine_multi_transform;
       ]

let () = run_test_tt_main suite
