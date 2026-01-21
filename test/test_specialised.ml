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

let test_invertible_true _ =
  let data =
    [|
      [| 6.; 4.; 4.; 4. |];
      [| 5.; 5.; 7.; 6. |];
      [| 4.; -9.; 3.; -7. |];
      [| 9.; 1.; 7.; -6. |];
    |]
  in
  let a1 = Specialised.of_array data in
  let resd = Specialised.determinant a1 in
  let resi = Specialised.invertible a1 in
  almost_equal (-2120.) resd;
  assert_equal true resi

let test_invertible_false _ =
  let data =
    [|
      [| 4.; 2.; -2.; -3. |];
      [| 9.; 6.; 2.; 6. |];
      [| 0.; -5.; 1.; -5. |];
      [| 0.; 0.; 0.; 0. |];
    |]
  in
  let a1 = Specialised.of_array data in
  let resd = Specialised.determinant a1 in
  let resi = Specialised.invertible a1 in
  almost_equal 0. resd;
  assert_equal false resi

let test_inverse_4x4_1 _ =
  let data =
    [|
      [| -5.; 2.; 6.; -8. |];
      [| 1.; -5.; 1.; 8. |];
      [| 7.; 7.; -6.; -7. |];
      [| 1.; -3.; 7.; 4. |];
    |]
  in
  let a = Specialised.of_array data in
  let b = Specialised.inverse a in
  let c = Specialised.cofactor a in
  almost_equal 532. (Specialised.determinant a);
  almost_equal (-160.) (Specialised.cell c (2, 3));
  almost_equal (-160. /. 532.) (Specialised.cell b (3, 2));
  almost_equal 105. (Specialised.cell c (3, 2));
  almost_equal (105. /. 532.) (Specialised.cell b (2, 3));
  let expected_data =
    [|
      [| 0.21805; 0.45113; 0.24060; -0.04511 |];
      [| -0.80827; -1.45677; -0.44361; 0.52068 |];
      [| -0.07895; -0.22368; -0.05263; 0.19737 |];
      [| -0.52256; -0.81391; -0.30075; 0.30639 |];
    |]
  in
  (* Weak comparison due to above numbers coming from a book *)
  (* let expected = Matrix.v expected_data in
  assert_bool "is equal" (Matrix.is_equal expected b) *)
  for j = 0 to 3 do
    for i = 0 to 3 do
      let e = expected_data.(j).(i) in
      let r = Specialised.cell b (j, i) in
      let diff = Float.abs (e -. r) < 0.00001 in
      assert_bool "" diff
    done
  done

let test_inverse_4x4_2 _ =
  let data =
    [|
      [| 8.; -5.; 9.; 2. |];
      [| 7.; 5.; 6.; 1. |];
      [| -6.; 0.; 9.; 6. |];
      [| -3.; 0.; -9.; -4. |];
    |]
  in
  let a = Specialised.of_array data in
  let b = Specialised.inverse a in
  let expected_data =
    [|
      [| -0.15385; -0.15385; -0.28205; -0.53846 |];
      [| -0.07692; 0.12308; 0.02564; 0.03077 |];
      [| 0.35897; 0.35897; 0.43590; 0.92308 |];
      [| -0.69231; -0.69231; -0.76923; -1.92308 |];
    |]
  in
  for j = 0 to 3 do
    for i = 0 to 3 do
      let e = expected_data.(j).(i) in
      let r = Specialised.cell b (j, i) in
      let diff = Float.abs (e -. r) < 0.00001 in
      assert_bool (Printf.sprintf "%f v %f" e r) diff
    done
  done

let test_inverse_4x4_3 _ =
  let data =
    [|
      [| 9.; 3.; 0.; 9. |];
      [| -5.; -2.; -6.; -3. |];
      [| -4.; 9.; 6.; 4. |];
      [| -7.; 6.; 6.; 2. |];
    |]
  in
  let a = Specialised.of_array data in
  let b = Specialised.inverse a in
  let expected_data =
    [|
      [| -0.04074; -0.07778; 0.14444; -0.22222 |];
      [| -0.07778; 0.03333; 0.36667; -0.33333 |];
      [| -0.02901; -0.14630; -0.10926; 0.12963 |];
      [| 0.17778; 0.06667; -0.26667; 0.33333 |];
    |]
  in
  for j = 0 to 3 do
    for i = 0 to 3 do
      let e = expected_data.(j).(i) in
      let r = Specialised.cell b (j, i) in
      let diff = Float.abs (e -. r) < 0.00001 in
      assert_bool (Printf.sprintf "%f v %f" e r) diff
    done
  done

let test_multiply_by_inverse _ =
  let data1 =
    [|
      [| 3.; -9.; 7.; 3. |];
      [| 3.; -8.; 2.; -9. |];
      [| -4.; 4.; 4.; 1. |];
      [| -6.; 5.; -1.; 1. |];
    |]
  in
  let a = Specialised.of_array data1 in
  let data2 =
    [|
      [| 8.; 2.; 2.; 2. |];
      [| 3.; -1.; 7.; 0. |];
      [| 7.; 0.; 5.; 4. |];
      [| 6.; -2.; 0.; 5. |];
    |]
  in
  let b = Specialised.of_array data2 in
  let c = Specialised.multiply a b in
  let ib = Specialised.inverse b in
  let res = Specialised.multiply c ib in
  assert_bool "is equal" (Specialised.is_equal res a)

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
         "Test invertible true" >:: test_invertible_true;
         "Test invertible false" >:: test_invertible_false;
         "Test inverse 4x4 1" >:: test_inverse_4x4_1;
         "Test inverse 4x4 2" >:: test_inverse_4x4_2;
         "Test inverse 4x4 3" >:: test_inverse_4x4_3;
         "Test multiply by inverse" >:: test_multiply_by_inverse;
       ]

let () = run_test_tt_main suite
