open Raybook
open OUnit2

let almost_equal a b =
  assert_bool "is almost equal" (Float.abs (a -. b) < Float.epsilon)

let test_create_tuple _ =
  let res : Tuple.t = Tuple.v 4.3 (-4.2) 3.1 1.0 in
  assert_equal 4.3 (Tuple.x res);
  assert_equal (-4.2) (Tuple.y res);
  assert_equal 3.1 (Tuple.z res);
  assert_equal 1.0 (Tuple.w res);
  assert_bool "is point" (Tuple.is_point res);
  assert_bool "is vector" (not (Tuple.is_vector res))

let test_create_point _ =
  (* Note the weirdness around 0.3 and 0.1 +. 0.2 is to
  trigger a failure when just using == on floating point. *)
  let res : Tuple.t = Tuple.point 4.3 (-4.2) 0.3 in
  let expected = Tuple.v 4.3 (-4.2) (0.1 +. 0.2) 1.0 in
  assert_bool "is equal" (Tuple.is_equal expected res);
  assert_bool "is point" (Tuple.is_point res);
  assert_bool "is vector" (not (Tuple.is_vector res))

let test_create_vector _ =
  (* Note the weirdness around 0.3 and 0.1 +. 0.2 is to
  trigger a failure when just using == on floating point. *)
  let res : Tuple.t = Tuple.vector 4.3 (-4.2) 0.3 in
  let expected = Tuple.v 4.3 (-4.2) (0.1 +. 0.2) 0.0 in
  assert_bool "is equal" (Tuple.is_equal expected res);
  assert_bool "is point" (not (Tuple.is_point res));
  assert_bool "is vector" (Tuple.is_vector res)

let test_sum_point_and_vector _ =
  let a1 = Tuple.point 3.0 (-2.0) 5.0 and a2 = Tuple.vector (-2.0) 3.0 1.0 in
  let res = Tuple.add a1 a2 in
  let expected = Tuple.point 1.0 1.0 6.0 in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_sum_vector_and_vector _ =
  let a1 = Tuple.vector 3.0 (-2.0) 5.0 and a2 = Tuple.vector (-2.0) 3.0 1.0 in
  let res = Tuple.add a1 a2 in
  let expected = Tuple.vector 1.0 1.0 6.0 in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_sum_point_and_point _ =
  let a1 = Tuple.point 3.0 (-2.0) 5.0 and a2 = Tuple.point (-2.0) 3.0 1.0 in
  assert_raises (Invalid_argument "Cannot add two points") (fun () ->
      let _ = Tuple.add a1 a2 in
      ())

let test_sub_point_and_point _ =
  let a1 = Tuple.point 3. 2. 1. and a2 = Tuple.point 5. 6. 7. in
  let res = Tuple.subtract a1 a2 in
  let expected = Tuple.vector (-2.) (-4.) (-6.) in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_sub_point_and_vector _ =
  let a1 = Tuple.point 3. 2. 1. and a2 = Tuple.vector 5. 6. 7. in
  let res = Tuple.subtract a1 a2 in
  let expected = Tuple.point (-2.) (-4.) (-6.) in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_sub_vector_and_vector _ =
  let a1 = Tuple.vector 3. 2. 1. and a2 = Tuple.vector 5. 6. 7. in
  let res = Tuple.subtract a1 a2 in
  let expected = Tuple.vector (-2.) (-4.) (-6.) in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_sub_vector_and_point _ =
  let a1 = Tuple.vector 3. 2. 1. and a2 = Tuple.point 5. 6. 7. in
  assert_raises (Invalid_argument "Cannot subtract point from vector")
    (fun () ->
      let _ = Tuple.subtract a1 a2 in
      ())

let test_negate_vector _ =
  let a = Tuple.vector (-1.) 2. (-3.) in
  let res = Tuple.negate a in
  let expected = Tuple.vector 1. (-2.) 3. in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_negate_point _ =
  let a = Tuple.point (-1.) 2. (-3.) in
  assert_raises (Invalid_argument "Cannot negate point") (fun () ->
      let _ = Tuple.negate a in
      ())

let test_multiply_vector _ =
  let a = Tuple.vector 1. (-2.) 3. in
  let res = Tuple.fmultiply a 3.5 in
  let expected = Tuple.vector 3.5 (-7.) 10.5 in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_multiply_point _ =
  let a = Tuple.point (-1.) 2. (-3.) in
  assert_raises (Invalid_argument "Cannot multiply point") (fun () ->
      let _ = Tuple.fmultiply a 3.5 in
      ())

let test_divide_vector _ =
  let a = Tuple.vector 1. (-2.) 3. in
  let res = Tuple.fdivide a 2. in
  let expected = Tuple.vector 0.5 (-1.) 1.5 in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_divide_point _ =
  let a = Tuple.point (-1.) 2. (-3.) in
  assert_raises (Invalid_argument "Cannot divide point") (fun () ->
      let _ = Tuple.fdivide a 2. in
      ())

let test_magnitude_of_vector_positive _ =
  let a = Tuple.vector 1. 2. 3. in
  let res = Tuple.magnitude a in
  let expected = Float.sqrt 14. in
  almost_equal expected res

let test_magnitude_of_vector_negative _ =
  let a = Tuple.vector (-1.) (-2.) (-3.) in
  let res = Tuple.magnitude a in
  let expected = Float.sqrt 14. in
  almost_equal expected res

let test_magnitude_of_point _ =
  let a = Tuple.point 1. 2. 3. in
  assert_raises (Invalid_argument "Cannot take magnitude of point") (fun () ->
      let _ = Tuple.magnitude a in
      ())

let test_normalize_vector_1 _ =
  let a = Tuple.vector 4. 0. 0. in
  let res = Tuple.normalize a in
  let expected = Tuple.vector 1. 0. 0. in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_normalize_vector_2 _ =
  let a = Tuple.vector 1. 2. 3. in
  let res = Tuple.normalize a in
  let mag = Float.sqrt 14. in
  let expected = Tuple.vector (1. /. mag) (2. /. mag) (3. /. mag) in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_normalize_point _ =
  let a = Tuple.point 1. 2. 3. in
  assert_raises (Invalid_argument "Cannot normalize point") (fun () ->
      let _ = Tuple.normalize a in
      ())

let test_magnitude_of_normalized_vector _ =
  let a = Tuple.vector 1. 2. 3. in
  let norm = Tuple.normalize a in
  let res = Tuple.magnitude norm in
  let expected = 1. in
  almost_equal expected res

let test_dot_vector_with_vector _ =
  let a1 = Tuple.vector 1. 2. 3. and a2 = Tuple.vector 2. 3. 4. in
  let res = Tuple.dot a1 a2 in
  let expected = 20. in
  almost_equal expected res

let test_dot_vector_with_point _ =
  let a1 = Tuple.vector 1. 2. 3. and a2 = Tuple.point 2. 3. 4. in
  assert_raises (Invalid_argument "Cannot use points in dot product") (fun () ->
      let _ = Tuple.dot a1 a2 in
      ())

let test_dot_point_with_vector _ =
  let a1 = Tuple.point 1. 2. 3. and a2 = Tuple.vector 2. 3. 4. in
  assert_raises (Invalid_argument "Cannot use points in dot product") (fun () ->
      let _ = Tuple.dot a1 a2 in
      ())

let test_dot_point_with_point _ =
  let a1 = Tuple.point 1. 2. 3. and a2 = Tuple.point 2. 3. 4. in
  assert_raises (Invalid_argument "Cannot use points in dot product") (fun () ->
      let _ = Tuple.dot a1 a2 in
      ())

let test_cross_vector_with_vector_1 _ =
  let a1 = Tuple.vector 1. 2. 3. and a2 = Tuple.vector 2. 3. 4. in
  let res = Tuple.cross a1 a2 in
  let expected = Tuple.vector (-1.) 2. (-1.) in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_cross_vector_with_vector_2 _ =
  let a1 = Tuple.vector 1. 2. 3. and a2 = Tuple.vector 2. 3. 4. in
  let res = Tuple.cross a2 a1 in
  let expected = Tuple.vector 1. (-2.) 1. in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_cross_vector_with_point _ =
  let a1 = Tuple.vector 1. 2. 3. and a2 = Tuple.point 2. 3. 4. in
  assert_raises (Invalid_argument "Cannot use points in cross product")
    (fun () ->
      let _ = Tuple.cross a1 a2 in
      ())

let test_cross_point_with_vector _ =
  let a1 = Tuple.point 1. 2. 3. and a2 = Tuple.vector 2. 3. 4. in
  assert_raises (Invalid_argument "Cannot use points in cross product")
    (fun () ->
      let _ = Tuple.cross a1 a2 in
      ())

let test_cross_point_with_point _ =
  let a1 = Tuple.point 1. 2. 3. and a2 = Tuple.point 2. 3. 4. in
  assert_raises (Invalid_argument "Cannot use points in cross product")
    (fun () ->
      let _ = Tuple.cross a1 a2 in
      ())

let test_vector_of_matrix _ =
  let data = [| [| 1. |]; [| 2. |]; [| 3. |]; [| 0. |] |] in
  let m = Matrix.v data in
  let res = Tuple.of_matrix m in
  let expected = Tuple.vector 1. 2. 3. in
  assert_equal expected res

let test_point_of_matrix _ =
  let data = [| [| 1. |]; [| 2. |]; [| 3. |]; [| 1. |] |] in
  let m = Matrix.v data in
  let res = Tuple.of_matrix m in
  let expected = Tuple.point 1. 2. 3. in
  assert_equal expected res

let test_tuple_of_invalid_w_matrix _ =
  let data = [| [| 1. |]; [| 2. |]; [| 3. |]; [| 3. |] |] in
  let m = Matrix.v data in
  assert_raises (Invalid_argument "W should be 0. or 1. for Vector or Point")
    (fun () ->
      let _ = Tuple.of_matrix m in
      ())

let test_tuple_of_invalid_size_matrix _ =
  let data = [| [| 1. |]; [| 2. |]; [| 3. |] |] in
  let m = Matrix.v data in
  assert_raises (Invalid_argument "Matrix expected to have 4x1 dimensions")
    (fun () ->
      let _ = Tuple.of_matrix m in
      ())

let test_reflect_45_degrees _ =
  let v = Tuple.vector 1. (-1.) 0. and n = Tuple.vector 0. 1. 0. in
  let res = Tuple.reflect v n in
  let expected = Tuple.vector 1. 1. 0. in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_refect_slanted_surface _ =
  let v = Tuple.vector 0. (-1.) 0. in
  let x = Float.sqrt 2. /. 2. in
  let n = Tuple.vector x x 0. in
  let res = Tuple.reflect v n in
  let expected = Tuple.vector 1. 0. 0. in
  assert_bool "is equal" (Tuple.is_equal expected res)

let suite =
  "Tuple tests"
  >::: [
         "Test create tuple" >:: test_create_tuple;
         "Test create point" >:: test_create_point;
         "Test create vector" >:: test_create_vector;
         "Test point plus vector" >:: test_sum_point_and_vector;
         "Test vector plus vector" >:: test_sum_vector_and_vector;
         "Test point plus point" >:: test_sum_point_and_point;
         "Test point sub point" >:: test_sub_point_and_point;
         "Test point sub vector" >:: test_sub_point_and_vector;
         "Test vector sub vector" >:: test_sub_vector_and_vector;
         "Test vector sub point" >:: test_sub_vector_and_point;
         "Test negate vector" >:: test_negate_vector;
         "Test negate point" >:: test_negate_point;
         "Test multiply vector" >:: test_multiply_vector;
         "Test multiply point" >:: test_multiply_point;
         "Test divide vector" >:: test_divide_vector;
         "Test divide point" >:: test_divide_point;
         "Test magnitude of positive vector"
         >:: test_magnitude_of_vector_positive;
         "Test magnitude of negative vector"
         >:: test_magnitude_of_vector_negative;
         "Test magnitude of point" >:: test_magnitude_of_point;
         "Test normalize vector 1" >:: test_normalize_vector_1;
         "Test normalize vector 2" >:: test_normalize_vector_2;
         "Test normalize point" >:: test_normalize_point;
         "Test magnitied of normalized vector"
         >:: test_magnitude_of_normalized_vector;
         "Test dot of vector with vector" >:: test_dot_vector_with_vector;
         "Test dot of vector with point" >:: test_dot_vector_with_point;
         "Test dot of point with vector" >:: test_dot_point_with_vector;
         "Test dot of point with point" >:: test_dot_point_with_point;
         "Test cross of vector with vector 1"
         >:: test_cross_vector_with_vector_1;
         "Test cross of vector with vector 2"
         >:: test_cross_vector_with_vector_2;
         "Test cross of vector with point" >:: test_cross_vector_with_point;
         "Test cross of point with vector" >:: test_cross_point_with_vector;
         "Test cross of point with point" >:: test_cross_point_with_point;
         "Test vector of matrix" >:: test_vector_of_matrix;
         "Test point of matrix" >:: test_point_of_matrix;
         "Test tuple of matrix invalid w" >:: test_tuple_of_invalid_w_matrix;
         "Test tuple of matrix invalid size"
         >:: test_tuple_of_invalid_size_matrix;
         "Test reflect tuple at 45 degrees" >:: test_reflect_45_degrees;
         "Test reflect tuple on slanted surface" >:: test_refect_slanted_surface;
       ]

let () = run_test_tt_main suite
