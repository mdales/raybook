open Raybook
open OUnit2

let almost_equal a b =
  assert_bool
    (Printf.sprintf "%f vs %f" a b)
    (Float.abs (a -. b) < Float.epsilon)

let test_ray_intersects_infinite_cylinder =
  let c =
    Shape.(
      v
        (Cylinder
           { min = Float.neg_infinity; max = Float.infinity; capped = false }))
  in
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

let test_ray_intersects_capped_cylinder =
  let c = Shape.(v (Cylinder { min = 1.; max = 2.; capped = true })) in
  let testcases =
    [
      ( "hit 1",
        Ray.v (Tuple.point 0. 3. 0.)
          (Tuple.normalize (Tuple.vector 0. (-1.) 0.)),
        2 );
      ( "hit 2",
        Ray.v (Tuple.point 0. 3. (-2.))
          (Tuple.normalize (Tuple.vector 0. (-1.) 2.)),
        2 );
      ( "hit 3",
        Ray.v (Tuple.point 0. 4. (-2.))
          (Tuple.normalize (Tuple.vector 0. (-1.) 1.)),
        2 );
      ( "hit 4",
        Ray.v (Tuple.point 0. 0. (-2.))
          (Tuple.normalize (Tuple.vector 0. 1. 2.)),
        2 );
      ( "hit 5",
        Ray.v
          (Tuple.point 0. (-1.) (-2.))
          (Tuple.normalize (Tuple.vector 0. 1. 1.)),
        2 );
    ]
  in
  List.map
    (fun (name, r, expected_count) ->
      name >:: fun _ ->
      let il = Intersection.intersects c r in
      assert_equal expected_count (List.length il))
    testcases

let test_ray_intersects_constrained_cylinder =
  let c = Shape.(v (Cylinder { min = 1.; max = 2.; capped = false })) in
  let testcases =
    [
      ( "hit 1",
        Ray.v (Tuple.point 0. 1.5 0.) (Tuple.normalize (Tuple.vector 0.1 1. 0.)),
        0 );
      ("hit 2", Ray.v (Tuple.point 0. 3. (-5.)) (Tuple.vector 0. 0. 1.), 0);
      ("hit 3", Ray.v (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.), 0);
      ("hit 4", Ray.v (Tuple.point 0. 2. (-5.)) (Tuple.vector 0. 0. 1.), 0);
      ("hit 5", Ray.v (Tuple.point 0. 1. (-5.)) (Tuple.vector 0. 0. 1.), 0);
      ("hit 6", Ray.v (Tuple.point 0. 1.5 (-2.)) (Tuple.vector 0. 0. 1.), 2);
    ]
  in
  List.map
    (fun (name, r, expected_count) ->
      name >:: fun _ ->
      let il = Intersection.intersects c r in
      assert_equal expected_count (List.length il))
    testcases

let test_ray_misses_cylinder =
  let c =
    Shape.(
      v
        (Cylinder
           { min = Float.neg_infinity; max = Float.infinity; capped = false }))
  in
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
  let c =
    Shape.(
      v
        (Cylinder
           { min = Float.neg_infinity; max = Float.infinity; capped = false }))
  in
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

let test_normal_at_capped =
  let c = Shape.(v (Cylinder { min = 1.; max = 2.; capped = true })) in
  let testcases =
    [
      ("test 1", Tuple.point 0. 1. 0., Tuple.vector 0. (-1.) 0.);
      ("test 2", Tuple.point 0.5 1. 0., Tuple.vector 0. (-1.) 0.);
      ("test 3", Tuple.point 0. 1. 0.5, Tuple.vector 0. (-1.) 0.);
      ("test 4", Tuple.point 0. 2. 0., Tuple.vector 0. 1. 0.);
      ("test 5", Tuple.point 0.5 2. 0., Tuple.vector 0. 1. 0.);
      ("test 6", Tuple.point 0. 2. 0.5, Tuple.vector 0. 1. 0.);
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
         "Test ray intersects infinite cylinder"
         >::: test_ray_intersects_infinite_cylinder;
         "Test ray intersects constrained cylinder"
         >::: test_ray_intersects_constrained_cylinder;
         "Test ray intersects caps" >::: test_ray_intersects_capped_cylinder;
         "Test ray misses cylinder" >::: test_ray_misses_cylinder;
         "Test normal at" >::: test_normal_at;
         "Test normal at with caps" >::: test_normal_at_capped;
       ]

let () = run_test_tt_main suite
