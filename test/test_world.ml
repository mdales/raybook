open Raybook
open OUnit2

let almost_equal a b =
  assert_bool
    (Printf.sprintf "%f vs %f" a b)
    (Float.abs (a -. b) < Float.epsilon)

let test_create_world _ =
  let l = Light.v (Tuple.point 10. 10. 10.) (Colour.v 0.2 0.3 0.4)
  and sl = [ Shape.(v Sphere) ] in
  let w = World.v l sl in
  assert_equal l (World.light w);
  assert_equal sl (World.shapes w)

let default_test_world ?(lighting = None) ?(ambient_high = false)
    ?(more_shapes = []) () =
  let l =
    match lighting with
    | None -> Light.v (Tuple.point (-10.) 10. (-10.)) (Colour.v 1. 1. 1.)
    | Some l -> l
  in
  let p = Pattern.(v (Solid (Colour.v 0.8 1.0 0.6))) in
  let m1 =
    Material.v ~pattern:p ~diffuse:0.7 ~specular:0.2
      ~ambient:(if ambient_high then 1.0 else 0.1)
      ()
  in
  let s1 = Shape.v ~material:m1 Shape.Sphere in
  let t = Transformation.scaling 0.5 0.5 0.5 in
  let m2 =
    Material.v
      ~pattern:Pattern.(v (Solid Colour.white))
      ~ambient:(if ambient_high then 1.0 else 0.1)
      ()
  in
  let s2 = Shape.v ~transform:t ~material:m2 Shape.Sphere in
  World.v l (s1 :: s2 :: more_shapes)

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

let test_intersect_single_plane_from_above _ =
  let m1 =
    Material.v
      ~pattern:Pattern.(v (Solid Colour.white))
      ~transparency:0.5 ~refractive_index:1.5 ()
  in
  let floor = Shape.(v ~material:m1 Plane) in
  let l = Light.v (Tuple.point (-10.) 10. (-10.)) (Colour.v 1. 1. 1.) in
  let w = World.v l [ floor ] in
  let r = Ray.v (Tuple.point 0. 1. 0.) (Tuple.vector 0. (-1.) 0.) in
  let il = World.intersect w r in
  assert_equal 1 (List.length il);
  let i = List.hd il in
  almost_equal 1. (Intersection.distance i);
  let comp = Precomputed.v i r il in
  assert_bool "over is more positive"
    (Precomputed.over_point comp > Precomputed.point comp);
  assert_bool "under is more negative"
    (Precomputed.point comp > Precomputed.under_point comp)

let test_intersect_single_plane_from_below _ =
  let m1 =
    Material.v
      ~pattern:Pattern.(v (Solid Colour.white))
      ~transparency:0.5 ~refractive_index:1.5 ()
  in
  let floor = Shape.(v ~material:m1 Plane) in
  let l = Light.v (Tuple.point (-10.) 10. (-10.)) (Colour.v 1. 1. 1.) in
  let w = World.v l [ floor ] in
  let r = Ray.v (Tuple.point 0. (-1.) 0.) (Tuple.vector 0. 1. 0.) in
  let il = World.intersect w r in
  assert_equal 1 (List.length il);
  let i = List.hd il in
  almost_equal 1. (Intersection.distance i);
  let comp = Precomputed.v i r il in
  assert_bool "under is more positive"
    (Precomputed.under_point comp > Precomputed.point comp);
  assert_bool "over is more negative"
    (Precomputed.point comp > Precomputed.over_point comp)

let test_shading_outside_intersection _ =
  let w = default_test_world () in
  let s = List.nth (World.shapes w) 0 in
  let r = Ray.v (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.) in
  let i = Intersection.v s 4. in
  let c = Precomputed.v i r [ i ] in
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
  let c = Precomputed.v i r [ i ] in
  let res = World.shader_hit w c in
  (* Originally no shadow, but now in shadow *)
  let expected =
    Colour.v 0.90498447208325749624 0.90498447208325749624
      0.90498447208325749624
  in
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
  let s1 = Shape.(v Sphere) in
  let t2 = Transformation.translation 0. 0. 10. in
  let s2 = Shape.v ~transform:t2 Shape.Sphere in
  let w = World.v l [ s1; s2 ] in
  let r = Ray.v (Tuple.point 0. 0. 5.) (Tuple.vector 0. 0. 1.) in
  let i = Intersection.v s2 4. in
  let comps = Precomputed.v i r [ i ] in
  let res = World.shader_hit w comps in
  let expected = Colour.v 0.1 0.1 0.1 in
  assert_bool "is equal" (Colour.is_equal expected res)

let test_reflected_colour_on_non_reflective_surface _ =
  let w = default_test_world ~ambient_high:true () in
  let r = Ray.v (Tuple.point 0. 0. 0.) (Tuple.vector 0. 0. 1.) in
  let s = List.nth (World.shapes w) 1 in
  let i = Intersection.v s 1. in
  let comps = Precomputed.v i r [ i ] in
  let res = World.reflected_colour w comps in
  assert_bool "is equal" (Colour.is_equal Colour.black res)

let test_reflected_colour_on_reflective_surface _ =
  let m =
    Material.v ~pattern:Pattern.(v (Solid Colour.white)) ~reflectivity:0.5 ()
  in
  let t = Transformation.translation 0. (-1.) 0. in
  let p = Shape.(v ~transform:t ~material:m Plane) in
  let w = default_test_world ~more_shapes:[ p ] () in
  let x = Float.sqrt 2. /. 2. in
  let r = Ray.v (Tuple.point 0. 0. (-2.)) (Tuple.vector 0. (0. -. x) x) in
  let i = Intersection.v p x in
  let comps = Precomputed.v i r [ i ] in
  let res = World.reflected_colour w comps in
  let expected =
    Colour.v 00.19033059654055548005 0.23791324567569432924
      0.14274794740541657534
  in
  assert_bool "is equal" (Colour.is_equal expected res)

let test_shader_hit_on_reflective_surface _ =
  let m =
    Material.v ~pattern:Pattern.(v (Solid Colour.white)) ~reflectivity:0.5 ()
  in
  let t = Transformation.translation 0. (-1.) 0. in
  let p = Shape.(v ~transform:t ~material:m Plane) in
  let w = default_test_world ~more_shapes:[ p ] () in
  let x = Float.sqrt 2. /. 2. in
  let r = Ray.v (Tuple.point 0. 0. (-2.)) (Tuple.vector 0. (0. -. x) x) in
  let i = Intersection.v p x in
  let comps = Precomputed.v i r [ i ] in
  let res = World.shader_hit w comps in
  let expected =
    Colour.v 0.85257168739730504470 0.90015433653244381063
      0.80498903826216605673
  in
  assert_bool "is equal" (Colour.is_equal expected res)

let test_reflection_between_parallel_surfaces_terminates _ =
  let t1 = Transformation.translation 0. 1. 0. in
  let m1 =
    Material.v ~pattern:Pattern.(v (Solid Colour.white)) ~reflectivity:1. ()
  in
  let p1 = Shape.(v ~transform:t1 ~material:m1 Plane) in
  let t2 = Transformation.translation 0. (-1.) 0. in
  let p2 = Shape.(v ~transform:t2 ~material:m1 Plane) in
  let l = Light.v (Tuple.point 0. 0. 0.) Colour.white in
  let w = World.v l [ p1; p2 ] in
  let r = Ray.v (Tuple.point 0. 0. 0.) (Tuple.vector 0. 1. 0.) in
  let _ = World.colour_at w r in
  ()

let test_refracted_colour_on_opaque_surface _ =
  let w = default_test_world () in
  let s = List.hd (World.shapes w) in
  let r = Ray.v (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.) in
  let il = [ Intersection.v s 4.; Intersection.v s 6. ] in
  let comps = Precomputed.v (List.hd il) r il in
  let res = World.refracted_colour ~count:1 w comps in
  let expected = Colour.black in
  assert_bool "is equal" (Colour.is_equal expected res)

let test_refracted_colour_on_recurrsion_limit _ =
  let m =
    Material.v
      ~pattern:Pattern.(v (Solid Colour.white))
      ~transparency:1. ~refractive_index:1.5 ()
  in
  let s = Shape.(v ~material:m Sphere) in
  let l = Light.v (Tuple.point (-10.) 10. (-10.)) (Colour.v 1. 1. 1.) in
  let w = World.v l [ s ] in
  let s = List.hd (World.shapes w) in
  let r = Ray.v (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.) in
  let il = [ Intersection.v s 4.; Intersection.v s 6. ] in
  let comps = Precomputed.v (List.hd il) r il in
  let res = World.refracted_colour ~count:0 w comps in
  let expected = Colour.black in
  assert_bool "is equal" (Colour.is_equal expected res)

let test_total_internal_reflection _ =
  let m =
    Material.v
      ~pattern:Pattern.(v (Solid Colour.white))
      ~transparency:1. ~refractive_index:1.5 ()
  in
  let s = Shape.(v ~material:m Sphere) in
  let l = Light.v (Tuple.point (-10.) 10. (-10.)) (Colour.v 1. 1. 1.) in
  let w = World.v l [ s ] in
  let x = Float.sqrt 2. /. 2. in
  let r = Ray.v (Tuple.point 0. 0. x) (Tuple.vector 0. 1. 0.) in
  let il = [ Intersection.v s (0. -. x); Intersection.v s x ] in
  let comps = Precomputed.v (List.nth il 1) r il in
  let res = World.refracted_colour ~count:1 w comps in
  let expected = Colour.black in
  assert_bool "is equal" (Colour.is_equal expected res)

let test_refracted_colour _ =
  let p = Pattern.(v TestPattern) in
  let m1 =
    Material.v ~pattern:p ~transparency:0. ~ambient:1. ~diffuse:0. ~specular:0.
      ()
  in
  let a = Shape.(v ~material:m1 Sphere) in
  let t = Transformation.scaling 0.5 0.5 0.5 in
  let m2 =
    Material.v
      ~pattern:Pattern.(v (Solid Colour.white))
      ~transparency:1. ~refractive_index:1.5 ()
  in
  let b = Shape.(v ~material:m2 ~transform:t Sphere) in
  let l = Light.v (Tuple.point (-10.) 10. (-10.)) (Colour.v 1. 1. 1.) in
  let w = World.v l [ a; b ] in
  let r = Ray.v (Tuple.point 0. 0. 0.1) (Tuple.vector 0. 1. 0.) in
  let il = World.intersect w r in
  let i = List.nth il 2 in
  let comps = Precomputed.v i r il in
  let res = World.refracted_colour ~count:5 w comps in
  let expected =
    Colour.v 0.00000000000000000000 0.99888470374667837071
      0.04721597844915045167
  in
  assert_bool "is equal" (Colour.is_equal expected res)

let test_shader_hit_with_refraction _ =
  let m1 =
    Material.v
      ~pattern:Pattern.(v (Solid Colour.white))
      ~transparency:0.5 ~refractive_index:1.5 ()
  in
  let t1 = Transformation.translation 0. (-1.) 0. in
  let floor = Shape.(v ~material:m1 ~transform:t1 Plane) in
  let m2 =
    Material.v
      ~pattern:Pattern.(v (Solid (Colour.v 1. 0. 0.)))
      ~diffuse:0. ~specular:0. ~ambient:0.5 ~shininess:0. ()
  in
  let t2 = Transformation.translation 0. (-3.5) (-0.5) in
  let ball = Shape.(v ~material:m2 ~transform:t2 Sphere) in
  let l = Light.v (Tuple.point (-10.) 10. (-10.)) (Colour.v 1. 1. 1.) in
  let w = World.v l [ floor; ball ] in
  let x = Float.sqrt 2. /. 2. in
  let r = Ray.v (Tuple.point 0. 0. (-3.)) (Tuple.vector 0. (0. -. x) x) in
  let i = Intersection.v floor (Float.sqrt 2.) in
  let comps = Precomputed.v i r [ i ] in
  let res = World.shader_hit ~count:5 w comps in
  let expected =
    Colour.v 0.93642538898150140536 0.68642538898150140536
      0.68642538898150140536
  in
  assert_bool "is equal" (Colour.is_equal expected res)

let test_shader_hit_with_relective_transparent_model _ =
  let m1 =
    Material.v
      ~pattern:Pattern.(v (Solid Colour.white))
      ~reflectivity:0.5 ~transparency:0.5 ~refractive_index:1.5 ()
  in
  let t1 = Transformation.translation 0. (-1.) 0. in
  let floor = Shape.(v ~material:m1 ~transform:t1 Plane) in
  let m2 =
    Material.v
      ~pattern:Pattern.(v (Solid (Colour.v 1. 0. 0.)))
      ~diffuse:0. ~specular:0. ~ambient:0.5 ~shininess:0. ()
  in
  let t2 = Transformation.translation 0. (-3.5) (-0.5) in
  let ball = Shape.(v ~material:m2 ~transform:t2 Sphere) in
  let w = default_test_world ~more_shapes:[ floor; ball ] () in
  let x = Float.sqrt 2. /. 2. in
  let r = Ray.v (Tuple.point 0. 0. (-3.)) (Tuple.vector 0. (0. -. x) x) in
  let i = Intersection.v floor (Float.sqrt 2.) in
  let comps = Precomputed.v i r [ i ] in
  let res = World.shader_hit ~count:5 w comps in
  let expected =
    Colour.v 0.93391514055019775320 0.69643422629373807897
      0.69243069136884338732
  in
  assert_bool "is equal" (Colour.is_equal expected res)

let suite =
  "World tests"
  >::: [
         "Test create world" >:: test_create_world;
         "Test default world" >:: test_default_world;
         "Test intersect plane from above"
         >:: test_intersect_single_plane_from_above;
         "Test intersect plane from below"
         >:: test_intersect_single_plane_from_below;
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
         "Test reflected colour on non-reflective surface"
         >:: test_reflected_colour_on_non_reflective_surface;
         "Test reflected colour on reflective surface"
         >:: test_reflected_colour_on_reflective_surface;
         "Test shader hit on reflective surface"
         >:: test_shader_hit_on_reflective_surface;
         "Test reflections between parallel surfaces terminates"
         >:: test_reflection_between_parallel_surfaces_terminates;
         "Test refracted colour on opaque surface"
         >:: test_refracted_colour_on_opaque_surface;
         "Test refracted colour at recursion limit"
         >:: test_refracted_colour_on_recurrsion_limit;
         "Test total internal reflection" >:: test_total_internal_reflection;
         "Test refracted colour" >:: test_refracted_colour;
         "Test shader hit with refraction" >:: test_shader_hit_with_refraction;
         "Test shader hit with reflection and transparecy"
         >:: test_shader_hit_with_relective_transparent_model;
       ]

let () = run_test_tt_main suite
