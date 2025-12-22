open Raybook
open OUnit2

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
  let res = Tuple.sum a1 a2 in
  let expected = Tuple.point 1.0 1.0 6.0 in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_sum_vector_and_vector _ =
  let a1 = Tuple.vector 3.0 (-2.0) 5.0 and a2 = Tuple.vector (-2.0) 3.0 1.0 in
  let res = Tuple.sum a1 a2 in
  let expected = Tuple.vector 1.0 1.0 6.0 in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_sum_point_and_point _ =
  let a1 = Tuple.point 3.0 (-2.0) 5.0 and a2 = Tuple.point (-2.0) 3.0 1.0 in
  assert_raises (Invalid_argument "Cannot add two points") (fun () ->
      let _ = Tuple.sum a1 a2 in
      ())

let test_sub_point_and_point _ =
  let a1 = Tuple.point 3. 2. 1. and a2 = Tuple.point 5. 6. 7. in
  let res = Tuple.sub a1 a2 in
  let expected = Tuple.vector (-2.) (-4.) (-6.) in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_sub_point_and_vector _ =
  let a1 = Tuple.point 3. 2. 1. and a2 = Tuple.vector 5. 6. 7. in
  let res = Tuple.sub a1 a2 in
  let expected = Tuple.point (-2.) (-4.) (-6.) in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_sub_vector_and_vector _ =
  let a1 = Tuple.vector 3. 2. 1. and a2 = Tuple.vector 5. 6. 7. in
  let res = Tuple.sub a1 a2 in
  let expected = Tuple.vector (-2.) (-4.) (-6.) in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_sub_vector_and_point _ =
  let a1 = Tuple.vector 3. 2. 1. and a2 = Tuple.point 5. 6. 7. in
  assert_raises (Invalid_argument "Cannot subtract point from vector")
    (fun () ->
      let _ = Tuple.sub a1 a2 in
      ())

let test_negate_vector _ =
  let a = Tuple.vector (-1.) 2. (-3.) in
  let res = Tuple.negate a in
  let expected = Tuple.vector 1. (-2.) 3. in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_negate_point _ =
  let a = Tuple.point (-1.) 2. (-3.) in
  assert_raises (Invalid_argument "Cannot negate point")
    (fun () ->
      let _ = Tuple.negate a in
      ())

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
       ]

let () = run_test_tt_main suite
