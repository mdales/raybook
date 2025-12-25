open Raybook
open OUnit2

let test_point_translation _ =
  let t = Transformation.translation 5. (-3.) 2. in
  let p = Tuple.point (-3.) 4. 5. in
  let pm = Tuple.to_matrix p in
  let resm = Matrix.multiply t pm in
  let rest = Tuple.of_matrix resm in
  let expected = Tuple.point 2. 1. 7. in
  assert_bool "is equal" (Tuple.is_equal expected rest)

let test_point_inverse_translation _ =
  let t = Transformation.translation 5. (-3.) 2. in
  let it = Matrix.inverse t in
  let p = Tuple.point (-3.) 4. 5. in
  let pm = Tuple.to_matrix p in
  let resm = Matrix.multiply it pm in
  let rest = Tuple.of_matrix resm in
  let expected = Tuple.point (-8.) 7. 3. in
  assert_bool "is equal" (Tuple.is_equal expected rest)

let test_vector_translation _ =
  let t = Transformation.translation 5. (-3.) 2. in
  let v = Tuple.vector (-3.) 4. 5. in
  let vm = Tuple.to_matrix v in
  let resm = Matrix.multiply t vm in
  let rest = Tuple.of_matrix resm in
  assert_bool "is equal" (Tuple.is_equal v rest)

let test_point_scaling _ =
  let t = Transformation.scaling 2. 3. 4. in
  let p = Tuple.point (-4.) 6. 8. in
  let pm = Tuple.to_matrix p in
  let resm = Matrix.multiply t pm in
  let rest = Tuple.of_matrix resm in
  let expected = Tuple.point (-8.) 18. 32. in
  assert_bool "is equal" (Tuple.is_equal expected rest)

let test_vector_scaling _ =
  let t = Transformation.scaling 2. 3. 4. in
  let v = Tuple.vector (-4.) 6. 8. in
  let vm = Tuple.to_matrix v in
  let resm = Matrix.multiply t vm in
  let rest = Tuple.of_matrix resm in
  let expected = Tuple.vector (-8.) 18. 32. in
  assert_bool "is equal" (Tuple.is_equal expected rest)

let test_vector_inverse_scaling _ =
  let t = Transformation.scaling 2. 3. 4. in
  let it = Matrix.inverse t in
  let v = Tuple.vector (-4.) 6. 8. in
  let vm = Tuple.to_matrix v in
  let resm = Matrix.multiply it vm in
  let rest = Tuple.of_matrix resm in
  let expected = Tuple.vector (-2.) 2. 2. in
  assert_bool "is equal" (Tuple.is_equal expected rest)

let test_point_reflection _ =
  let t = Transformation.scaling (-1.) 1. 1. in
  let p = Tuple.point 2. 3. 4. in
  let pm = Tuple.to_matrix p in
  let resm = Matrix.multiply t pm in
  let rest = Tuple.of_matrix resm in
  let expected = Tuple.point (-2.) 3. 4. in
  assert_bool "is equal" (Tuple.is_equal expected rest)

let test_rotate_point_on_x _ =
  let p = Tuple.point 0. 1. 0. in
  let pm = Tuple.to_matrix p in
  let half_quarter = Transformation.rotate_x (Float.pi /. 4.)
  and full_quarter = Transformation.rotate_x (Float.pi /. 2.) in
  let half_resm = Matrix.multiply half_quarter pm
  and full_resm = Matrix.multiply full_quarter pm in
  let half_rest = Tuple.of_matrix half_resm
  and full_rest = Tuple.of_matrix full_resm in
  let half_expected = Tuple.point 0. (Float.sqrt 2. /. 2.) (Float.sqrt 2. /. 2.)
  and full_expected = Tuple.point 0. 0. 1. in
  assert_bool "is equal" (Tuple.is_equal half_expected half_rest);
  assert_bool "is equal" (Tuple.is_equal full_expected full_rest)

let test_inverse_rotate_point_on_x _ =
  let p = Tuple.point 0. 1. 0. in
  let pm = Tuple.to_matrix p in
  let half_quarter = Transformation.rotate_x (Float.pi /. 4.) in
  let inv_half_quarter = Matrix.inverse half_quarter in
  let half_resm = Matrix.multiply inv_half_quarter pm in
  let half_rest = Tuple.of_matrix half_resm in
  let half_expected =
    Tuple.point 0. (Float.sqrt 2. /. 2.) (Float.sqrt 2. /. -2.)
  in
  assert_bool "is equal" (Tuple.is_equal half_expected half_rest)

let test_rotate_point_on_y _ =
  let p = Tuple.point 0. 0. 1. in
  let pm = Tuple.to_matrix p in
  let half_quarter = Transformation.rotate_y (Float.pi /. 4.)
  and full_quarter = Transformation.rotate_y (Float.pi /. 2.) in
  let half_resm = Matrix.multiply half_quarter pm
  and full_resm = Matrix.multiply full_quarter pm in
  let half_rest = Tuple.of_matrix half_resm
  and full_rest = Tuple.of_matrix full_resm in
  let half_expected = Tuple.point (Float.sqrt 2. /. 2.) 0. (Float.sqrt 2. /. 2.)
  and full_expected = Tuple.point 1. 0. 0. in
  assert_bool "is equal" (Tuple.is_equal half_expected half_rest);
  assert_bool "is equal" (Tuple.is_equal full_expected full_rest)

let test_rotate_point_on_z _ =
  let p = Tuple.point 0. 1. 0. in
  let pm = Tuple.to_matrix p in
  let half_quarter = Transformation.rotate_z (Float.pi /. 4.)
  and full_quarter = Transformation.rotate_z (Float.pi /. 2.) in
  let half_resm = Matrix.multiply half_quarter pm
  and full_resm = Matrix.multiply full_quarter pm in
  let half_rest = Tuple.of_matrix half_resm
  and full_rest = Tuple.of_matrix full_resm in
  let half_expected =
    Tuple.point (Float.sqrt 2. /. -2.) (Float.sqrt 2. /. 2.) 0.
  and full_expected = Tuple.point (-1.) 0. 0. in
  assert_bool "is equal" (Tuple.is_equal half_expected half_rest);
  assert_bool "is equal" (Tuple.is_equal full_expected full_rest)

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
       ]

let () = run_test_tt_main suite
