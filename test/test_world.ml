open Raybook
open OUnit2

let test_create_world _ =
  let l = Light.v (Tuple.point 10. 10. 10.) (Colour.v 0.2 0.3 0.4)
  and sl = [ Shape.Sphere (Sphere.v ()) ] in
  let w = World.v l sl in
  assert_equal l (World.light w);
  assert_equal sl (World.shapes w)

(* let default_test_world _ =
    let l = Light.v (Tuple.point (-10.) 10. (-10.)) (Colour.v 1. 1. 1.) in
    let m1 = Material.v ~colour:(Colour.v 0.8 1.0 0.6) ~diffuse:0.7 ~specular:0.2 in
    let s1 = Shape.Sphere (Sphere.v ~material:m1 ()) in
    let t = Transformation.scaling 0.5 0.5 0.5 in
    let s2 = Shape.Sphere (Sphere.v ~transform:t ())
    and sl = [ Shape.Sphere (Sphere.v ()) ] in
    () *)

let suite = "World tests" >::: [ "Test create world" >:: test_create_world ]
let () = run_test_tt_main suite
