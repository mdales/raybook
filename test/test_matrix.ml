open Raybook
open OUnit2

let almost_equal a b =
  assert_bool
    (Printf.sprintf "%f vs %f" a b)
    (Float.abs (a -. b) < Float.epsilon)

let test_create_matrix_4x4 _ =
  let data =
    [|
      [| 1.; 2.; 3.; 4. |];
      [| 5.5; 6.5; 7.5; 8.5 |];
      [| 9.; 10.; 11.; 12. |];
      [| 13.5; 14.5; 16.5; 17.6 |];
    |]
  in
  let res = Matrix.v data in
  almost_equal 1. (Matrix.cell res (0, 0));
  almost_equal 4. (Matrix.cell res (0, 3));
  almost_equal 5.5 (Matrix.cell res (1, 0));
  almost_equal 7.5 (Matrix.cell res (1, 2));
  almost_equal 11. (Matrix.cell res (2, 2));
  almost_equal 13.5 (Matrix.cell res (3, 0));
  almost_equal 16.5 (Matrix.cell res (3, 2))

let test_create_matrix_3x3 _ =
  let data = [| [| -3.; 5.; 0. |]; [| 1.; -2.; -7. |]; [| 0.; 1.; 1. |] |] in
  let res = Matrix.v data in
  almost_equal (-3.) (Matrix.cell res (0, 0));
  almost_equal (-2.) (Matrix.cell res (1, 1));
  almost_equal 1. (Matrix.cell res (2, 2))

let test_create_matrix_2x2 _ =
  let data = [| [| -3.; 5. |]; [| 1.; -2. |] |] in
  let res = Matrix.v data in
  almost_equal (-3.) (Matrix.cell res (0, 0));
  almost_equal (-2.) (Matrix.cell res (1, 1));
  almost_equal 5. (Matrix.cell res (0, 1));
  almost_equal 1. (Matrix.cell res (1, 0))

let test_create_invalid_matrix_1 _ =
  let data =
    [| [| 1.; 2.; 3.; 4. |]; [| 5.5; 6.5; 7.5 |]; [| 9.; 10. |]; [| 13.5 |] |]
  in
  assert_raises (Invalid_argument "Array rows must all have same length")
    (fun () ->
      let _ = Matrix.v data in
      ())

let test_create_invalid_matrix_2 _ =
  let data : float array array = [| [||] |] in
  assert_raises (Invalid_argument "Array dimensions can not be zero") (fun () ->
      let _ = Matrix.v data in
      ())

let test_create_invalid_matrix_3 _ =
  let data : float array array = [||] in
  assert_raises (Invalid_argument "Array dimensions can not be zero") (fun () ->
      let _ = Matrix.v data in
      ())

let test_matrix_equality_1 _ =
  let data =
    [|
      [| 1.; 2.; 3.; 4. |]; [| 5.5; 6.5; 7.5; 8.5 |]; [| 9.; 10.; 11.; 12. |];
    |]
  in
  let a1 = Matrix.v data in
  let res = Matrix.is_equal a1 a1 in
  assert_equal true res

let test_matrix_equality_2 _ =
  let data =
    [|
      [| 1.; 2.; 3. |];
      [| 5.5; 6.5; 7.5 |];
      [| 9.; 10.; 11. |];
      [| 13.5; 14.5; 16.5 |];
    |]
  in
  let a1 = Matrix.v data in
  let res = Matrix.is_equal a1 a1 in
  assert_equal true res

let test_matrix_equality_3 _ =
  let data1 =
    [|
      [| 1.; 2.; 3. |];
      [| 5.5; 6.5; 7.5 |];
      [| 9.; 10.; 11. |];
      [| 13.5; 14.5; 16.5 |];
    |]
  in
  let a1 = Matrix.v data1 in
  let data2 =
    [|
      [| 1.; 2.; 3.; 4. |]; [| 5.5; 6.5; 7.5; 8.5 |]; [| 9.; 10.; 11.; 12. |];
    |]
  in
  let a2 = Matrix.v data2 in
  let res = Matrix.is_equal a1 a2 in
  assert_equal false res

let test_matrix_equality_4 _ =
  let data1 =
    [|
      [| 1.; 2.; 3.; 4. |]; [| 5.5; 7.5; 7.5; 8.5 |]; [| 9.; 10.; 11.; 12. |];
    |]
  in
  let a1 = Matrix.v data1 in
  let data2 =
    [|
      [| 1.; 2.; 3.; 4. |]; [| 5.5; 6.5; 7.5; 8.5 |]; [| 9.; 10.; 11.; 12. |];
    |]
  in
  let a2 = Matrix.v data2 in
  let res = Matrix.is_equal a1 a2 in
  assert_equal false res

let test_multiply_1 _ =
  let data1 =
    [|
      [| 1.; 2.; 3.; 4. |];
      [| 5.; 6.; 7.; 8. |];
      [| 9.; 8.; 7.; 6. |];
      [| 5.; 4.; 3.; 2. |];
    |]
  in
  let a1 = Matrix.v data1 in
  let data2 =
    [|
      [| -2.; 1.; 2.; 3. |];
      [| 3.; 2.; 1.; -1. |];
      [| 4.; 3.; 6.; 5. |];
      [| 1.; 2.; 7.; 8. |];
    |]
  in
  let a2 = Matrix.v data2 in
  let data_expected =
    [|
      [| 20.; 22.; 50.; 48. |];
      [| 44.; 54.; 114.; 108. |];
      [| 40.; 58.; 110.; 102. |];
      [| 16.; 26.; 46.; 42. |];
    |]
  in
  let expected = Matrix.v data_expected in
  let res = Matrix.multiply a1 a2 in
  assert_bool "is equal" (Matrix.is_equal expected res)

let test_multiply_by_tuple _ =
  let data1 =
    [|
      [| 1.; 2.; 3.; 4. |];
      [| 2.; 4.; 4.; 2. |];
      [| 8.; 6.; 4.; 1. |];
      [| 0.; 0.; 0.; 1. |];
    |]
  in
  let a1 = Matrix.v data1 in
  let t = Tuple.point 1. 2. 3. in
  let a2 = Tuple.to_matrix t in
  let resm = Matrix.multiply a1 a2 in
  let rest = Tuple.of_matrix resm in
  let expected = Tuple.point 18. 24. 33. in
  assert_equal expected rest

let suite =
  "Matrix tests"
  >::: [
         "Test create matrix 4x4" >:: test_create_matrix_4x4;
         "Test create matrix 3x3" >:: test_create_matrix_3x3;
         "Test create matrix 2x2" >:: test_create_matrix_2x2;
         "Test create invalid matrix 1" >:: test_create_invalid_matrix_1;
         "Test create invalid matrix 2" >:: test_create_invalid_matrix_2;
         "Test create invalid matrix 3" >:: test_create_invalid_matrix_3;
         "Test matrix equality 1" >:: test_matrix_equality_1;
         "Test matrix equality 2" >:: test_matrix_equality_2;
         "Test matrix equality 3" >:: test_matrix_equality_3;
         "Test matrix equality 4" >:: test_matrix_equality_4;
         "Test matrix multiply 1" >:: test_multiply_1;
         "Test multiply matrix by tuple" >:: test_multiply_by_tuple;
       ]

let () = run_test_tt_main suite
