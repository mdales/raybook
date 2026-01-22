open Raybook
open OUnit2

let almost_equal a b =
  assert_bool
    (Printf.sprintf "%f vs %f" a b)
    (Float.abs (a -. b) < Float.epsilon)

let test_ray_intersects_cone =
  let c =
    Shape.(
      v
        (Cone { min = Float.neg_infinity; max = Float.infinity; capped = false }))
  in
  let testcases =
    [
      ( "test 1",
        Ray.v
          (Specialised.point 0. 0. (-5.))
          (Specialised.normalize (Specialised.vector 0. 0. 1.)),
        5.,
        5. );
      ( "test 2",
        Ray.v
          (Specialised.point 0. 0. (-5.))
          (Specialised.normalize (Specialised.vector 1. 1. 1.)),
        8.66025401549264373102,
        8.66025406019612731257 );
      ( "test 3",
        Ray.v
          (Specialised.point 1. 1. (-5.))
          (Specialised.normalize (Specialised.vector (-0.5) (-1.) 1.)),
        4.55005567935635113486,
        49.44994432064366662871 );
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

let test_ray_parallel_intersects_cone _ =
  let c =
    Shape.(
      v
        (Cone { min = Float.neg_infinity; max = Float.infinity; capped = false }))
  in
  let r =
    Ray.v
      (Specialised.point 0. 0. (-1.))
      (Specialised.normalize (Specialised.vector 0. 1. 1.))
  in
  let il = Intersection.intersects c r in
  assert_equal 1 (List.length il);
  let t1 = 0.35355339059327378637 in
  almost_equal t1 (Intersection.distance (List.nth il 0))

let test_ray_intersects_capped_double_cone =
  let c = Shape.(v (Cone { min = -0.5; max = 2.; capped = true })) in
  let testcases =
    [
      ( "hit 1",
        Ray.v
          (Specialised.point 0. 0. (-5.))
          (Specialised.normalize (Specialised.vector 0. 1. 0.)),
        0 );
      ( "hit 2",
        Ray.v
          (Specialised.point 0. 0. (-0.25))
          (Specialised.normalize (Specialised.vector 0. 1. 1.)),
        2 );
      ( "hit 3",
        Ray.v
          (Specialised.point 0. 0. (-0.25))
          (Specialised.normalize (Specialised.vector 0. 1. 0.)),
        4 );
      ( "hit 4",
        Ray.v
          (Specialised.point 0. 0. (-1.8))
          (Specialised.normalize (Specialised.vector 0. 1. 0.)),
        2 );
    ]
  in
  List.map
    (fun (name, r, expected_count) ->
      name >:: fun _ ->
      let il = Intersection.intersects c r in
      assert_equal expected_count (List.length il))
    testcases

let test_ray_intersects_capped_single_cone =
  let c = Shape.(v (Cone { min = 0.5; max = 2.; capped = true })) in
  let testcases =
    [
      ( "hit 1",
        Ray.v
          (Specialised.point 0. 0. (-5.))
          (Specialised.normalize (Specialised.vector 0. 1. 0.)),
        0 );
      ( "hit 3",
        Ray.v
          (Specialised.point 0. 0. (-0.25))
          (Specialised.normalize (Specialised.vector 0. 1. 0.)),
        2 );
      ( "hit 4",
        Ray.v
          (Specialised.point 0. 0. (-1.8))
          (Specialised.normalize (Specialised.vector 0. 1. 0.)),
        2 );
    ]
  in
  List.map
    (fun (name, r, expected_count) ->
      name >:: fun _ ->
      let il = Intersection.intersects c r in
      assert_equal expected_count (List.length il))
    testcases

let test_normal_at =
  let c =
    Shape.(
      v
        (Cone { min = Float.neg_infinity; max = Float.infinity; capped = false }))
  in
  let x = Float.sqrt 2. *. -1. in
  let testcases =
    [
      ("test 1", Specialised.point 0. 0. 0., Specialised.vector 0. 0. 0.);
      ("test 2", Specialised.point 1. 1. 1., Specialised.vector 1. x 1.);
      ( "test 3",
        Specialised.point (-1.) (-1.) 0.,
        Specialised.vector (-1.) 1. 0. );
    ]
  in
  List.map
    (fun (name, point, normal) ->
      name >:: fun _ ->
      let res = Intersection.normal_at c point in
      assert_bool "is equal"
        (Specialised.is_equal (Specialised.normalize normal) res))
    testcases

let test_normal_at_capped_double_cone =
  let c = Shape.(v (Cone { min = -1.; max = 2.; capped = true })) in
  let testcases =
    [
      ("test 1", Specialised.point 0. (-1.) 0., Specialised.vector 0. (-1.) 0.);
      ("test 2", Specialised.point 0.5 (-1.) 0., Specialised.vector 0. (-1.) 0.);
      ("test 3", Specialised.point 0. (-1.) 0.5, Specialised.vector 0. (-1.) 0.);
      ("test 4", Specialised.point 0. 2. 0., Specialised.vector 0. 1. 0.);
      ("test 5", Specialised.point 0.5 2. 0., Specialised.vector 0. 1. 0.);
      ("test 6", Specialised.point 0. 2. 0.5, Specialised.vector 0. 1. 0.);
      ("test 7", Specialised.point 0. 2. 1.5, Specialised.vector 0. 1. 0.);
    ]
  in
  List.map
    (fun (name, point, normal) ->
      name >:: fun _ ->
      let res = Intersection.normal_at c point in
      assert_bool "is equal" (Specialised.is_equal normal res))
    testcases

let test_normal_at_capped_single_cone =
  let c = Shape.(v (Cone { min = 1.; max = 2.; capped = true })) in
  let testcases =
    [
      ("test 1", Specialised.point 0. 1. 0., Specialised.vector 0. (-1.) 0.);
      ("test 2", Specialised.point 0.5 1. 0., Specialised.vector 0. (-1.) 0.);
      ("test 3", Specialised.point 0. 1. 0.5, Specialised.vector 0. (-1.) 0.);
      ("test 4", Specialised.point 0. 2. 0., Specialised.vector 0. 1. 0.);
    ]
  in
  List.map
    (fun (name, point, normal) ->
      name >:: fun _ ->
      let res = Intersection.normal_at c point in
      assert_bool "is equal" (Specialised.is_equal normal res))
    testcases

let suite =
  "Cone tests"
  >::: [
         "Test ray intersects cone" >::: test_ray_intersects_cone;
         "Test ray parallel to cone" >:: test_ray_parallel_intersects_cone;
         "Test ray intersects capped double cone"
         >::: test_ray_intersects_capped_double_cone;
         "Test ray intersects capped single cone"
         >::: test_ray_intersects_capped_single_cone;
         "Test normal at" >::: test_normal_at;
         "Test normal at capped double" >::: test_normal_at_capped_double_cone;
         "Test normal at capped single" >::: test_normal_at_capped_single_cone;
       ]

let () = run_test_tt_main suite
