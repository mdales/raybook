open Raybook
open OUnit2

let almost_equal a b =
  assert_bool
    (Printf.sprintf "%f vs %f" a b)
    (Float.abs (a -. b) < Float.epsilon)

let test_create_camera _ =
  let res = Camera.v (160, 120) (Float.pi /. 2.) in
  assert_equal (160, 120) (Camera.dimensions res);
  assert_equal (Float.pi /. 2.) (Camera.field_of_view res);
  assert_bool "is equal"
    (Matrix.is_equal (Matrix.identity 4) (Camera.transform res))

let test_landscape_camera _ =
  let res = Camera.v (200, 125) (Float.pi /. 2.) in
  almost_equal 0.01 (Camera.pixel_size res)

let test_portrait_camera _ =
  let res = Camera.v (125, 200) (Float.pi /. 2.) in
  almost_equal 0.01 (Camera.pixel_size res)

let suite =
  "Camera tests"
  >::: [
         "Test create camera" >:: test_create_camera;
         "Test landscape pixel size" >:: test_landscape_camera;
         "Test portrait pixel size" >:: test_portrait_camera;
       ]

let () = run_test_tt_main suite
