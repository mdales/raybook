open Raybook
open OUnit2

let test_create_world _ =
  let l = Light.v (Tuple.point 10. 10. 10.) (Colour.v 0.2 0.3 0.4)
  and sl = [ Shape.Sphere (Sphere.v 42) ] in
  let w = World.v l sl in
  assert_equal l (World.light w);
  assert_equal sl (World.shapes w)

let suite = "World tests" >::: [ "Test create world" >:: test_create_world ]
let () = run_test_tt_main suite
