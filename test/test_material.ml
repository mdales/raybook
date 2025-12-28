open Raybook
open OUnit2

let test_create_material_defaults _ =
  let c = Colour.v 0.8 0.8 0.8 in
  let res = Material.v ~colour:c () in
  assert_equal 0.1 (Material.ambient res);
  assert_bool "is equal" (Colour.is_equal c (Material.colour res));
  assert_equal 0.9 (Material.diffuse res);
  assert_equal 0.9 (Material.specular res);
  assert_equal 200.0 (Material.shininess res)

let test_create_material_non_defaults _ =
  let c = Colour.v 0.8 0.8 0.8 in
  let res =
    Material.v ~colour:c ~ambient:0.4 ~diffuse:0.5 ~specular:0.6
      ~shininess:123.0 ()
  in
  assert_equal 0.4 (Material.ambient res);
  assert_bool "is equal" (Colour.is_equal c (Material.colour res));
  assert_equal 0.5 (Material.diffuse res);
  assert_equal 0.6 (Material.specular res);
  assert_equal 123.0 (Material.shininess res)

let suite =
  "Material tests"
  >::: [
         "Test create material" >:: test_create_material_defaults;
         "Test create material non default"
         >:: test_create_material_non_defaults;
       ]

let () = run_test_tt_main suite
