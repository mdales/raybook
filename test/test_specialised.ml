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
    Specialised.of_array [|
        [| 1.; 0.; 0. ; 0. |];
        [| 0.; 1.; 0. ; 0. |];
        [| 0.; 0.; 1. ; 0. |];
        [| 0.; 0.; 0. ; 1. |];
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

let suite =
  "Specialised tests"
  >::: [
   "Test create specialised matrix" >:: test_create_specialised_4x4;
   "Test create specailised matrix of matrix" >:: test_create_specialised_of_matrix_4x4;
   "Test create specialised point" >:: test_create_specialised_point;
      "Test create specialised point of tuple" >:: test_create_specialised_point_of_tuple;
   "Test create specialised vector" >:: test_create_specialised_vector;
      "Test create specialised vector of tuple" >:: test_create_specialised_vector_of_tuple;
         "Test identity" >:: test_identity;
         "Test multiply matrix by tuple" >:: test_multiply_by_tuple;
         "Test multiply by identity" >:: test_multiply_matrix_with_identity;
         "Test multiply id by tuple" >:: test_multiply_identity_by_tuple;
          "Test transpose matrix" >:: test_transpose_matrix;
          "Test transpose identity" >:: test_transpose_indentity;



       ]

let () = run_test_tt_main suite
