open Raybook
open OUnit2

let almost_equal a b =
  assert_bool
    (Printf.sprintf "%f vs %f" a b)
    (Float.abs (a -. b) < Float.epsilon)

let test_ray_intersects_cylinder =
  let c = Shape.(v Cylinder) in
  let testcases =
    [
      ( "Tangent",
        Ray.v (Tuple.point 1. 0. (-5.)) (Tuple.vector 0. 0. 1.),
        5.,
        5. );
      ( "perpendicular strike",
        Ray.v (Tuple.point 0. 0.0 (-5.)) (Tuple.vector 0. 0. 1.),
        4.,
        6. );
      ( "angled strike",
        Ray.v (Tuple.point 0.5 0. (-5.))
          (Tuple.normalize (Tuple.vector 0.1 1. 1.)),
        6.80798191702731436692,
        7.08872343937886739695 );
    ]
  in
  List.map
    (fun (name, r, t1, t2) ->
      name >:: fun _ ->
      let il = Intersection.intersects c r in
      assert_equal 2 (List.length il);
      almost_equal t1 (Intersection.distance (List.nth il 0));
      almost_equal t2 (Intersection.distance (List.nth il 1)))
    testcases

let test_ray_misses_cylinder =
  let c = Shape.(v Cylinder) in
  let testcases =
    [
      ("aligned on edge", Ray.v (Tuple.point 1. 0. 0.) (Tuple.vector 0. 1. 0.));
      ("inside along axis", Ray.v (Tuple.point 0. 0. 0.) (Tuple.vector 0. 1. 0.));
      ( "outside/away",
        Ray.v (Tuple.point 0. 0. (-5.))
          (Tuple.normalize (Tuple.vector 1. 1. 1.)) );
    ]
  in
  List.map
    (fun (name, r) ->
      name >:: fun _ ->
      let il = Intersection.intersects c r in
      assert_equal 0 (List.length il))
    testcases

let test_normal_at =
  let c = Shape.(v Cylinder) in
  let testcases =
    [
      ("normal +ve x face", Tuple.point 1. 0. 0., Tuple.vector 1. 0. 0.);
      ("normal -ve x face", Tuple.point 0. 5. (-1.), Tuple.vector 0. 0. (-1.));
      ("normal +ve z face", Tuple.point 0. (-2.) 1., Tuple.vector 0. 0. 1.);
      ("normal -ve z face", Tuple.point (-1.) 1. 0., Tuple.vector (-1.) 0. 0.);
    ]
  in
  List.map
    (fun (name, point, normal) ->
      name >:: fun _ ->
      let res = Intersection.normal_at c point in
      assert_bool "is equal" (Tuple.is_equal normal res))
    testcases

let suite =
  "Cylinder tests"
  >::: [
         "Test ray intersects cylinder" >::: test_ray_intersects_cylinder;
         "Test ray misses cylinder" >::: test_ray_misses_cylinder;
         "Test normal at" >::: test_normal_at;
       ]

let () = run_test_tt_main suite
