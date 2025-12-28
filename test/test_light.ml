open Raybook
open OUnit2

let test_create_light _ =
  let p = Tuple.point 0. 0. 0. and i = Colour.v 1. 1. 1. in
  let res = Light.v p i in
  assert_bool "is equal" (Tuple.is_equal p (Light.position res));
  assert_bool "is equal" (Colour.is_equal i (Light.intensity res))

let test_create_light_with_vector_fails _ =
  let p = Tuple.vector 0. 0. 0. and i = Colour.v 1. 1. 1. in
  assert_raises (Invalid_argument "Position must be a point") (fun () ->
      let _ = Light.v p i in
      ())

let suite =
  "Light tests"
  >::: [
         "Test create light" >:: test_create_light;
         "Test create light fails wtih vector"
         >:: test_create_light_with_vector_fails;
       ]

let () = run_test_tt_main suite
