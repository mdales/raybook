open Raybook
open OUnit2

let test_create_material_defaults _ =
  let c = Colour.v 0.8 0.8 0.8 in
  let p = Pattern.Solid c in
  let res = Material.v ~pattern:p () in
  assert_equal 0.1 (Material.ambient res);
  assert_equal p (Material.pattern res);
  assert_equal 0.9 (Material.diffuse res);
  assert_equal 0.9 (Material.specular res);
  assert_equal 200.0 (Material.shininess res)

let test_create_material_non_defaults _ =
  let c = Colour.v 0.8 0.8 0.8 in
  let p = Pattern.Solid c in
  let res =
    Material.v ~pattern:p ~ambient:0.4 ~diffuse:0.5 ~specular:0.6
      ~shininess:123.0 ()
  in
  assert_equal 0.4 (Material.ambient res);
  assert_equal p (Material.pattern res);
  assert_equal 0.5 (Material.diffuse res);
  assert_equal 0.6 (Material.specular res);
  assert_equal 123.0 (Material.shininess res)

let test_eye_between_light_and_material _ =
  let c = Colour.v 1. 1. 1. in
  let p = Pattern.Solid c in
  let material = Material.v ~pattern:p () in
  let point = Tuple.point 0. 0. 0. in

  let eye = Tuple.vector 0. 0. (-1.) in
  let normal = Tuple.vector 0. 0. (-1.) in
  let light = Light.v (Tuple.point 0. 0. (-10.)) (Colour.v 1. 1. 1.) in
  let shadow = false in

  let res = Light.lighting ~material ~point ~eye ~normal ~light ~shadow () in
  let expected = Colour.v 1.9 1.9 1.9 in
  assert_bool "is equal" (Colour.is_equal expected res)

let test_eye_45_degrees_from_light_and_material _ =
  let c = Colour.v 1. 1. 1. in
  let material = Material.v ~pattern:(Pattern.Solid c) () in
  let point = Tuple.point 0. 0. 0. in

  let x = Float.sqrt 2. /. 2. in
  let eye = Tuple.vector 0. x (0. -. x) in
  let normal = Tuple.vector 0. 0. (-1.) in
  let light = Light.v (Tuple.point 0. 0. (-10.)) (Colour.v 1. 1. 1.) in
  let shadow = false in

  let res = Light.lighting ~material ~point ~eye ~normal ~light ~shadow () in
  let expected = Colour.v 1.0 1.0 1.0 in
  assert_bool "is equal" (Colour.is_equal expected res)

let test_light_45_degrees_from_eye_and_material _ =
  let c = Colour.v 1. 1. 1. in
  let material = Material.v ~pattern:(Pattern.Solid c) () in
  let point = Tuple.point 0. 0. 0. in

  let x = Float.sqrt 2. /. 2. in
  let y = 0.1 +. (0.9 *. x) in
  let eye = Tuple.vector 0. 0. (-1.) in
  let normal = Tuple.vector 0. 0. (-1.) in
  let light = Light.v (Tuple.point 0. 10. (-10.)) (Colour.v 1. 1. 1.) in
  let shadow = false in

  let res = Light.lighting ~material ~point ~eye ~normal ~light ~shadow () in
  let expected = Colour.v y y y in
  assert_bool "is equal" (Colour.is_equal expected res)

let test_eye_and_light_45_degrees_from_material _ =
  let c = Colour.v 1. 1. 1. in
  let material = Material.v ~pattern:(Pattern.Solid c) () in
  let point = Tuple.point 0. 0. 0. in

  let x = Float.sqrt 2. /. 2. in
  let y = 0.1 +. (0.9 *. x) +. 0.9 in
  let eye = Tuple.vector 0. (0. -. x) (0. -. x) in
  let normal = Tuple.vector 0. 0. (-1.) in
  let light = Light.v (Tuple.point 0. 10. (-10.)) (Colour.v 1. 1. 1.) in
  let shadow = false in

  let res = Light.lighting ~material ~point ~eye ~normal ~light ~shadow () in
  let expected = Colour.v y y y in
  assert_bool "is equal" (Colour.is_equal expected res)

let test_material_between_light_and_eye _ =
  let c = Colour.v 1. 1. 1. in
  let material = Material.v ~pattern:(Pattern.Solid c) () in
  let point = Tuple.point 0. 0. 0. in

  let eye = Tuple.vector 0. 0. (-1.) in
  let normal = Tuple.vector 0. 0. (-1.) in
  let light = Light.v (Tuple.point 0. 0. 10.) (Colour.v 1. 1. 1.) in
  let shadow = false in

  let res = Light.lighting ~material ~point ~eye ~normal ~light ~shadow () in
  let expected = Colour.v 0.1 0.1 0.1 in
  assert_bool "is equal" (Colour.is_equal expected res)

let test_surface_in_shadow _ =
  let c = Colour.v 1. 1. 1. in
  let material = Material.v ~pattern:(Pattern.Solid c) () in
  let point = Tuple.point 0. 0. 0. in

  let eye = Tuple.vector 0. 0. (-1.) in
  let normal = Tuple.vector 0. 0. (-1.) in
  let light = Light.v (Tuple.point 0. 0. (-10.)) (Colour.v 1. 1. 1.) in
  let shadow = true in

  let res = Light.lighting ~material ~point ~eye ~normal ~light ~shadow () in
  let expected = Colour.v 0.1 0.1 0.1 in
  assert_bool "is equal" (Colour.is_equal expected res)

let suite =
  "Material tests"
  >::: [
         "Test create material" >:: test_create_material_defaults;
         "Test create material non default"
         >:: test_create_material_non_defaults;
         "Test lighting with eye between light and surface"
         >:: test_eye_between_light_and_material;
         "Test lighting with eye at 45 degrees"
         >:: test_eye_45_degrees_from_light_and_material;
         "Test lighting with light at 45 degrees"
         >:: test_light_45_degrees_from_eye_and_material;
         "Test lighting both at 45 degrees"
         >:: test_eye_and_light_45_degrees_from_material;
         "Test lighting with material between eye and light"
         >:: test_material_between_light_and_eye;
         "Test surface in shadow" >:: test_surface_in_shadow;
       ]

let () = run_test_tt_main suite
