open Raybook
open OUnit2

let almost_equal a b =
  assert_bool
    (Printf.sprintf "%f vs %f" a b)
    (Float.abs (a -. b) < Float.epsilon)

let test_create_world _ =
  let l = Light.v (Tuple.point 10. 10. 10.) (Colour.v 0.2 0.3 0.4)
  and sl = [ Shape.Sphere (Sphere.v ()) ] in
  let w = World.v l sl in
  assert_equal l (World.light w);
  assert_equal sl (World.shapes w)

let default_test_world () =
  let l = Light.v (Tuple.point (-10.) 10. (-10.)) (Colour.v 1. 1. 1.) in
  let m1 =
    Material.v ~colour:(Colour.v 0.8 1.0 0.6) ~diffuse:0.7 ~specular:0.2 ()
  in
  let s1 = Shape.Sphere (Sphere.v ~material:m1 ()) in
  let t = Transformation.scaling 0.5 0.5 0.5 in
  let s2 = Shape.Sphere (Sphere.v ~transform:t ()) in
  World.v l [ s1; s2 ]

let test_default_world _ =
  let w = default_test_world () in
  assert_equal 2 (List.length (World.shapes w))

let test_intersect_world_with_ray _ =
  let w = default_test_world () in
  let r = Ray.v (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.) in
  let res = World.intersect w r in
  assert_equal 4 (List.length res);
  almost_equal 4. (Intersection.distance (List.nth res 0));
  almost_equal 4.5 (Intersection.distance (List.nth res 1));
  almost_equal 5.5 (Intersection.distance (List.nth res 2));
  almost_equal 6. (Intersection.distance (List.nth res 3))

let suite =
  "World tests"
  >::: [
         "Test create world" >:: test_create_world;
         "Test default world" >:: test_default_world;
         "Test intersect world with ray" >:: test_intersect_world_with_ray;
       ]

let () = run_test_tt_main suite
