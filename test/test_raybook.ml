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

let suite =
  "Tuple tests"
  >::: [
         "Test create tuple" >:: test_create_tuple;
         "Test create point" >:: test_create_point;
         "Test create vector" >:: test_create_vector;
       ]

let () = run_test_tt_main suite
