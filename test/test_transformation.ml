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

let suite =
  "Transformation tests"
  >::: [
         "Test point translation" >:: test_point_translation;
         "Test point translation inverse" >:: test_point_inverse_translation;
         "Test vector translation" >:: test_vector_translation;
         "Test point scaling" >:: test_point_scaling;
         "Test vector scaling" >:: test_vector_scaling;
         "Test vector translation inverse" >:: test_vector_inverse_scaling;
       ]

let () = run_test_tt_main suite
