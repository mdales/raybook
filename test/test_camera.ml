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
    (Specialised.is_equal (Specialised.identity ()) (Camera.transform res))

let test_landscape_camera _ =
  let res = Camera.v (200, 125) (Float.pi /. 2.) in
  almost_equal 0.01 (Camera.pixel_size res)

let test_portrait_camera _ =
  let res = Camera.v (125, 200) (Float.pi /. 2.) in
  almost_equal 0.01 (Camera.pixel_size res)

let test_ray_for_origin _ =
  let c = Camera.v (201, 101) (Float.pi /. 2.) in
  let res = Camera.ray_for_pixel c (100, 50) in
  let expected_origin = Specialised.point 0. 0. 0.
  and expected_direction = Specialised.vector 0. 0. (-1.) in
  assert_bool "is equal" (Specialised.is_equal expected_origin (Ray.origin res));
  assert_bool "is equal"
    (Specialised.is_equal expected_direction (Ray.direction res))

let test_ray_for_corner _ =
  let c = Camera.v (201, 101) (Float.pi /. 2.) in
  let res = Camera.ray_for_pixel c (0, 0) in
  let expected_origin = Specialised.point 0. 0. 0.
  and expected_direction =
    Specialised.vector 0.66518642611945077991 0.33259321305972538996
      (-0.66851235825004806657)
  in
  assert_bool "is equala"
    (Specialised.is_equal expected_origin (Ray.origin res));
  assert_bool "is equalb"
    (Specialised.is_equal expected_direction (Ray.direction res))

let test_ray_for_transform _ =
  let t =
    Specialised.multiply
      (Transformation.rotate_y (Float.pi /. 4.))
      (Transformation.translation 0. (-2.) 5.)
  in
  let c = Camera.v ~transform:t (201, 101) (Float.pi /. 2.) in
  let res = Camera.ray_for_pixel c (100, 50) in
  let x = Float.sqrt 2. /. 2. in
  let expected_origin = Specialised.point 0. 2. (-5.)
  and expected_direction = Specialised.vector x 0. (0. -. x) in
  assert_bool "is equal" (Specialised.is_equal expected_origin (Ray.origin res));
  assert_bool "is equal"
    (Specialised.is_equal expected_direction (Ray.direction res))

let suite =
  "Camera tests"
  >::: [
         "Test create camera" >:: test_create_camera;
         "Test landscape pixel size" >:: test_landscape_camera;
         "Test portrait pixel size" >:: test_portrait_camera;
         "Test ray through origin" >:: test_ray_for_origin;
         "Test ray through corner" >:: test_ray_for_corner;
         "Test ray through transform" >:: test_ray_for_transform;
       ]

let () = run_test_tt_main suite
