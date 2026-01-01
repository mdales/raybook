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

let default_test_world ?(lighting = None) ?(ambient_high = false) () =
  let l =
    match lighting with
    | None -> Light.v (Tuple.point (-10.) 10. (-10.)) (Colour.v 1. 1. 1.)
    | Some l -> l
  in
  let m1 =
    Material.v ~colour:(Colour.v 0.8 1.0 0.6) ~diffuse:0.7 ~specular:0.2
      ~ambient:(if ambient_high then 1.0 else 0.1)
      ()
  in
  let s1 = Shape.Sphere (Sphere.v ~material:m1 ()) in
  let t = Transformation.scaling 0.5 0.5 0.5 in
  let m2 =
    Material.v ~colour:(Colour.v 1. 1. 1.)
      ~ambient:(if ambient_high then 1.0 else 0.1)
      ()
  in
  let s2 = Shape.Sphere (Sphere.v ~transform:t ~material:m2 ()) in
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

let test_shading_outside_intersection _ =
  let w = default_test_world () in
  let s = List.nth (World.shapes w) 0 in
  let r = Ray.v (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.) in
  let i = Intersection.v s 4. in
  let c = Precomputed.v i r in
  let res = World.shader_hit w c in
  let expected =
    Colour.v 0.380661193081034355 0.475826491351292957 0.285495894810775752
  in
  assert_bool "is equal" (Colour.is_equal expected res)

let test_shading_inside_intersection _ =
  let lighting = Some (Light.v (Tuple.point 0. 0.25 0.) (Colour.v 1. 1. 1.)) in
  let w = default_test_world ~lighting () in
  let s = List.nth (World.shapes w) 1 in
  let r = Ray.v (Tuple.point 0. 0. 0.) (Tuple.vector 0. 0. 1.) in
  let i = Intersection.v s 0.5 in
  let c = Precomputed.v i r in
  let res = World.shader_hit w c in
  (* Originally no shadow, but now in shadow *)
  let expected = Colour.v 0.1 0.1 0.1 in
  assert_bool "is equal" (Colour.is_equal expected res)

let test_colour_at_misses _ =
  let w = default_test_world () in
  let r = Ray.v (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 1. 0.) in
  let res = World.colour_at w r in
  assert_bool "is equal" (Colour.is_equal (Colour.v 0. 0. 0.) res)

let test_colour_at_hits _ =
  let w = default_test_world () in
  let r = Ray.v (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.) in
  let res = World.colour_at w r in
  let expected =
    Colour.v 0.380661193081034355 0.475826491351292957 0.285495894810775752
  in
  assert_bool "is equal" (Colour.is_equal expected res)

let test_colour_at_hits_in_between _ =
  let w = default_test_world ~ambient_high:true () in
  let r = Ray.v (Tuple.point 0. 0. 0.75) (Tuple.vector 0. 0. (-1.)) in
  let res = World.colour_at w r in
  let expected = Colour.v 1. 1. 1. in
  assert_bool "is equal" (Colour.is_equal expected res)

let test_shadow_scenario_1 _ =
  (* Nothing is colinear *)
  let w = default_test_world () in
  let p = Tuple.point 0. 10. 0. in
  let res = World.is_shadowed w p in
  assert_equal false res

let test_shadow_scenario_2 _ =
  (* object betweek point and light *)
  let w = default_test_world () in
  let p = Tuple.point 10. (-10.) 10. in
  let res = World.is_shadowed w p in
  assert_equal true res

let test_shadow_scenario_3 _ =
  (* object behind light *)
  let w = default_test_world () in
  let p = Tuple.point (-20.) 20. (-20.) in
  let res = World.is_shadowed w p in
  assert_equal false res

let test_shadow_scenario_4 _ =
  (* object behind point *)
  let w = default_test_world () in
  let p = Tuple.point (-2.) 2. (-2.) in
  let res = World.is_shadowed w p in
  assert_equal false res

let test_shader_hit_is_given_intersection_in_shadow _ =
  let l = Light.v (Tuple.point 0. 0. (-10.)) (Colour.v 1. 1. 1.) in
  let s1 = Shape.Sphere (Sphere.v ()) in
  let t2 = Transformation.translation 0. 0. 10. in
  let s2 = Shape.Sphere (Sphere.v ~transform:t2 ()) in
  let w = World.v l [ s1; s2 ] in
  let r = Ray.v (Tuple.point 0. 0. 5.) (Tuple.vector 0. 0. 1.) in
  let i = Intersection.v s2 4. in
  let comps = Precomputed.v i r in
  let res = World.shader_hit w comps in
  let expected = Colour.v 0.1 0.1 0.1 in
  assert_bool "is equal" (Colour.is_equal expected res)

let suite =
  "World tests"
  >::: [
         "Test create world" >:: test_create_world;
         "Test default world" >:: test_default_world;
         "Test intersect world with ray" >:: test_intersect_world_with_ray;
         "Test shading outside intersection"
         >:: test_shading_outside_intersection;
         "Test shading inside intersection" >:: test_shading_inside_intersection;
         "Test colour at misses" >:: test_colour_at_misses;
         "Test colour at hits" >:: test_colour_at_hits;
         "Test colour at hits between" >:: test_colour_at_hits_in_between;
         "Test shadow scenarion 1" >:: test_shadow_scenario_1;
         "Test shadow scenarion 2" >:: test_shadow_scenario_2;
         "Test shadow scenarion 3" >:: test_shadow_scenario_3;
         "Test shadow scenarion 4" >:: test_shadow_scenario_4;
         "Test shader hit in shadow"
         >:: test_shader_hit_is_given_intersection_in_shadow;
       ]

let () = run_test_tt_main suite
