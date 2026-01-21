open Raybook
open OUnit2

let almost_equal a b =
  assert_bool
    (Printf.sprintf "%f vs %f" a b)
    (Float.abs (a -. b) < Float.epsilon)

let test_create_specialised_4x4 _ =
  let data =
    [|
      [| 1.; 2.; 3.; 4. |];
      [| 5.5; 6.5; 7.5; 8.5 |];
      [| 9.; 10.; 11.; 12. |];
      [| 13.5; 14.5; 16.5; 17.6 |];
    |]
  in
  let res = Specialised.of_array data in
  almost_equal 1. (Specialised.cell res (0, 0));
  almost_equal 4. (Specialised.cell res (0, 3));
  almost_equal 5.5 (Specialised.cell res (1, 0));
  almost_equal 7.5 (Specialised.cell res (1, 2));
  almost_equal 11. (Specialised.cell res (2, 2));
  almost_equal 13.5 (Specialised.cell res (3, 0));
  almost_equal 16.5 (Specialised.cell res (3, 2))

let test_create_specialised_of_matrix_4x4 _ =
  let data =
    [|
      [| 1.; 2.; 3.; 4. |];
      [| 5.5; 6.5; 7.5; 8.5 |];
      [| 9.; 10.; 11.; 12. |];
      [| 13.5; 14.5; 16.5; 17.6 |];
    |]
  in
  let res = Specialised.of_matrix (Matrix.v data) in
  almost_equal 1. (Specialised.cell res (0, 0));
  almost_equal 4. (Specialised.cell res (0, 3));
  almost_equal 5.5 (Specialised.cell res (1, 0));
  almost_equal 7.5 (Specialised.cell res (1, 2));
  almost_equal 11. (Specialised.cell res (2, 2));
  almost_equal 13.5 (Specialised.cell res (3, 0));
  almost_equal 16.5 (Specialised.cell res (3, 2))

let test_create_specialised_point _ =
  let res = Specialised.of_point 1. 2. 3. in
  almost_equal 1. (Specialised.cell res (0, 0));
  almost_equal 2. (Specialised.cell res (1, 0));
  almost_equal 3. (Specialised.cell res (2, 0));
  almost_equal 1. (Specialised.cell res (3, 0))

let test_create_specialised_point_of_tuple _ =
  let res = Specialised.of_tuple (Tuple.point 1. 2. 3.) in
  almost_equal 1. (Specialised.cell res (0, 0));
  almost_equal 2. (Specialised.cell res (1, 0));
  almost_equal 3. (Specialised.cell res (2, 0));
  almost_equal 1. (Specialised.cell res (3, 0))

let test_create_specialised_vector _ =
  let res = Specialised.of_vector 1. 2. 3. in
  almost_equal 1. (Specialised.cell res (0, 0));
  almost_equal 2. (Specialised.cell res (1, 0));
  almost_equal 3. (Specialised.cell res (2, 0));
  almost_equal 0. (Specialised.cell res (3, 0))

let test_create_specialised_vector_of_tuple _ =
  let res = Specialised.of_tuple (Tuple.vector 1. 2. 3.) in
  almost_equal 1. (Specialised.cell res (0, 0));
  almost_equal 2. (Specialised.cell res (1, 0));
  almost_equal 3. (Specialised.cell res (2, 0));
  almost_equal 0. (Specialised.cell res (3, 0))

let test_identity _ =
  let res = Specialised.identity () in
  let expected =
    Specialised.of_array
      [|
        [| 1.; 0.; 0.; 0. |];
        [| 0.; 1.; 0.; 0. |];
        [| 0.; 0.; 1.; 0. |];
        [| 0.; 0.; 0.; 1. |];
      |]
  in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_multiply_by_tuple _ =
  let data1 =
    [|
      [| 1.; 2.; 3.; 4. |];
      [| 2.; 4.; 4.; 2. |];
      [| 8.; 6.; 4.; 1. |];
      [| 0.; 0.; 0.; 1. |];
    |]
  in
  let a1 = Specialised.of_array data1 in
  let t = Specialised.of_point 1. 2. 3. in
  let res = Specialised.multiply a1 t in
  let expected = Specialised.of_point 18. 24. 33. in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_multiply_matrix_with_identity _ =
  let data1 =
    [|
      [| 1.; 2.; 3.; 4. |];
      [| 2.; 4.; 4.; 2. |];
      [| 8.; 6.; 4.; 1. |];
      [| 0.; 0.; 0.; 1. |];
    |]
  in
  let a1 = Specialised.of_array data1 in
  let a2 = Specialised.identity () in
  let res = Specialised.multiply a1 a2 in
  assert_bool "is equal" (Specialised.is_equal a1 res)

let test_multiply_identity_by_tuple _ =
  let a1 = Specialised.identity () in
  let t = Specialised.of_point 1. 2. 3. in
  let res = Specialised.multiply a1 t in
  assert_bool "is equal" (Specialised.is_equal t res)

let test_transpose_matrix _ =
  let data1 =
    [|
      [| 1.; 2.; 3.; 4. |];
      [| 2.; 4.; 4.; 2. |];
      [| 8.; 6.; 4.; 1. |];
      [| 4.; 8.; 3.; 0. |];
    |]
  in
  let a1 = Specialised.of_array data1 in
  let data2 =
    [|
      [| 1.; 2.; 8.; 4. |];
      [| 2.; 4.; 6.; 8. |];
      [| 3.; 4.; 4.; 3. |];
      [| 4.; 2.; 1.; 0. |];
    |]
  in
  let expected = Specialised.of_array data2 in
  let res = Specialised.transpose a1 in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_transpose_indentity _ =
  let expected = Specialised.identity () in
  let res = Specialised.transpose expected in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_submatrix_4x4 _ =
  let data =
    [|
      [| -6.; 1.; 1.; 6. |];
      [| -8.; 5.; 8.; 6. |];
      [| -1.; 0.; 8.; 2. |];
      [| -7.; 1.; -1.; 1. |];
    |]
  in
  let a1 = Specialised.of_array data in
  let res = Specialised.submatrix a1 (2, 1) in
  let expected_data =
    [| [| -6.; 1.; 6. |]; [| -8.; 8.; 6. |]; [| -7.; -1.; 1. |] |]
  in
  let expected = Specialised.of_array expected_data in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_submatrix_3x3 _ =
  let data = [| [| 1.; 5.; 0. |]; [| -3.; 2.; 7. |]; [| 0.; 6.; -3. |] |] in
  let a1 = Specialised.of_array data in
  let res = Specialised.submatrix a1 (0, 2) in
  let expected_data = [| [| -3.; 2. |]; [| 0.; 6. |] |] in
  let expected = Specialised.of_array expected_data in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_determinant_2x2 _ =
  let data = [| [| 1.; 5. |]; [| -3.; 2. |] |] in
  let a1 = Specialised.of_array data in
  let res = Specialised.determinant a1 in
  almost_equal 17. res

let test_minor_3x3 _ =
  let data = [| [| 3.; 5.; 0. |]; [| 2.; -1.; -7. |]; [| 6.; -1.; 5. |] |] in
  let a1 = Specialised.of_array data in
  let res = Specialised.minor a1 in
  for j = 0 to 2 do
    for i = 0 to 2 do
      let s = Specialised.submatrix a1 (j, i) in
      let d = Specialised.determinant s in
      let r = Specialised.cell res (j, i) in
      almost_equal d r
    done
  done

let test_cofactor_3x3 _ =
  let data = [| [| 3.; 5.; 0. |]; [| 2.; -1.; -7. |]; [| 6.; -1.; 5. |] |] in
  let a1 = Specialised.of_array data in
  let resm = Specialised.minor a1 in
  let resc = Specialised.cofactor a1 in
  almost_equal (-12.) (Specialised.cell resm (0, 0));
  almost_equal (-12.) (Specialised.cell resc (0, 0));
  almost_equal 25. (Specialised.cell resm (1, 0));
  almost_equal (-25.) (Specialised.cell resc (1, 0))

let test_determinant_3x3 _ =
  let data = [| [| 1.; 2.; 6. |]; [| -5.; 8.; -4. |]; [| 2.; 6.; 4. |] |] in
  let a1 = Specialised.of_array data in
  let resc = Specialised.cofactor a1 in
  let resd = Specialised.determinant a1 in
  almost_equal 56. (Specialised.cell resc (0, 0));
  almost_equal 12. (Specialised.cell resc (0, 1));
  almost_equal (-46.) (Specialised.cell resc (0, 2));
  almost_equal (-196.) resd

let test_determinant_4x4 _ =
  let data =
    [|
      [| -2.; -8.; 3.; 5. |];
      [| -3.; 1.; 7.; 3. |];
      [| 1.; 2.; -9.; 6. |];
      [| -6.; 7.; 7.; -9. |];
    |]
  in
  let a1 = Specialised.of_array data in
  let resc = Specialised.cofactor a1 in
  let resd = Specialised.determinant a1 in
  almost_equal 690. (Specialised.cell resc (0, 0));
  almost_equal 447. (Specialised.cell resc (0, 1));
  almost_equal 210. (Specialised.cell resc (0, 2));
  almost_equal 51. (Specialised.cell resc (0, 3));
  almost_equal (-4071.) resd

let suite =
  "Specialised tests"
  >::: [
         "Test create specialised matrix" >:: test_create_specialised_4x4;
         "Test create specailised matrix of matrix"
         >:: test_create_specialised_of_matrix_4x4;
         "Test create specialised point" >:: test_create_specialised_point;
         "Test create specialised point of tuple"
         >:: test_create_specialised_point_of_tuple;
         "Test create specialised vector" >:: test_create_specialised_vector;
         "Test create specialised vector of tuple"
         >:: test_create_specialised_vector_of_tuple;
         "Test identity" >:: test_identity;
         "Test multiply matrix by tuple" >:: test_multiply_by_tuple;
         "Test multiply by identity" >:: test_multiply_matrix_with_identity;
         "Test multiply id by tuple" >:: test_multiply_identity_by_tuple;
         "Test transpose matrix" >:: test_transpose_matrix;
         "Test transpose identity" >:: test_transpose_indentity;
         "Test submatrix 4x4" >:: test_submatrix_4x4;
         "Test submatrix 3x3" >:: test_submatrix_3x3;
         "Test determinant 2x2" >:: test_determinant_2x2;
         "Test minor 3x3" >:: test_minor_3x3;
         "Test cofactor 3x3" >:: test_cofactor_3x3;
         "Test determinant 3x3" >:: test_determinant_3x3;
         "Test determinant 4x4" >:: test_determinant_4x4;
       ]

let () = run_test_tt_main suite
