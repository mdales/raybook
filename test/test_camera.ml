open Raybook
open OUnit2

let test_create_camera _ =
    let res = Camera.v (160, 120) (Float.pi /. 2.) in
    assert_equal (160, 120) (Camera.dimensions res);
    assert_equal (Float.pi /. 2.) (Camera.field_of_view res);
    assert_bool "is equal" (Matrix.is_equal (Matrix.identity 4) (Camera.transform res))

let suite =
      "Camera tests"
      >::: [
             "Test create camera" >:: test_create_camera;
             ]

let () = run_test_tt_main suite
