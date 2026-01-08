open Raybook
open OUnit2

let almost_equal a b =
  assert_bool
    (Printf.sprintf "%f vs %f" a b)
    (Float.abs (a -. b) < Float.epsilon)

let test_create_cube_defaults _ =
  let s = Shape.(v Cube) in
  assert_equal Shape.Cube (Shape.geometry s);
  let expected_colour = Colour.v 1. 1. 1. in
  let expected_material =
    Material.v ~pattern:Pattern.(v (Solid expected_colour)) ()
  in
  assert_equal expected_material (Shape.material s);
  let ident = Matrix.identity 4 in
  assert_bool "is equal" (Matrix.is_equal ident (Shape.transform s));
  assert_bool "is equal" (Matrix.is_equal ident (Shape.inverse_transform s));
  assert_bool "is equal"
    (Matrix.is_equal ident (Shape.transpose_inverse_transform s))

let test_ray_intersects_cube =
  let c = Shape.(v Cube) in
  let testcases =
    [
      ( "from +x",
        Ray.v (Tuple.point 5. 0.5 0.) (Tuple.vector (-1.) 0. 0.),
        4.,
        6. );
      ( "from -x",
        Ray.v (Tuple.point (-5.) 0.5 0.) (Tuple.vector 1. 0. 0.),
        4.,
        6. );
      ( "from +y",
        Ray.v (Tuple.point 0.5 5. 0.) (Tuple.vector 0. (-1.) 0.),
        4.,
        6. );
      ( "from -y",
        Ray.v (Tuple.point 0.5 (-5.) 0.) (Tuple.vector 0. 1. 0.),
        4.,
        6. );
      ( "from +z",
        Ray.v (Tuple.point 0.5 0. 5.) (Tuple.vector 0. 0. (-1.)),
        4.,
        6. );
      ( "from -z",
        Ray.v (Tuple.point 0.5 0. (-5.)) (Tuple.vector 0. 0. 1.),
        4.,
        6. );
      ("inside", Ray.v (Tuple.point 0. 0.5 0.) (Tuple.vector 0. 0. 1.), -1., 1.);
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

let test_ray_misses_cube =
  let c = Shape.(v Cube) in
  let testcases =
    [
      ( "miss 1",
        Ray.v (Tuple.point (-2.) 0. 0.) (Tuple.vector 0.2673 0.5345 0.8018) );
      ( "miss 2",
        Ray.v (Tuple.point 0. (-2.) 0.) (Tuple.vector 0.8018 0.2673 0.5345) );
      ( "miss 3",
        Ray.v (Tuple.point 0. 0. (-2.)) (Tuple.vector 0.5345 0.8018 0.2673) );
      ("miss 4", Ray.v (Tuple.point 2. 0. 2.) (Tuple.vector 0. 0. (-1.)));
      ("miss 5", Ray.v (Tuple.point 0. 2. 2.) (Tuple.vector 0. (-1.) 0.));
      ("miss 6", Ray.v (Tuple.point 2. 2. 0.) (Tuple.vector (-1.) 0. 0.));
    ]
  in
  List.map
    (fun (name, r) ->
      name >:: fun _ ->
      let il = Intersection.intersects c r in
      assert_equal 0 (List.length il))
    testcases

let test_normal_at =
  let c = Shape.(v Cube) in
  let testcases =
    [
      ("normal +ve x face", Tuple.point 1. 0.5 (-0.8), Tuple.vector 1. 0. 0.);
      ( "normal -ve x face",
        Tuple.point (-1.) (-0.2) 0.9,
        Tuple.vector (-1.) 0. 0. );
      ("normal +ve y face", Tuple.point (-0.4) 1. (-0.1), Tuple.vector 0. 1. 0.);
      ( "normal -ve y face",
        Tuple.point 0.3 (-1.) (-0.7),
        Tuple.vector 0. (-1.) 0. );
      ("normal +ve z face", Tuple.point (-0.6) 0.3 1., Tuple.vector 0. 0. 1.);
      ("normal -ve z face", Tuple.point 0.4 0.4 (-1.), Tuple.vector 0. 0. (-1.));
      ("normal +ve corner", Tuple.point 1. 1. 1., Tuple.vector 1. 0. 0.);
      ( "normal -ve corner",
        Tuple.point (-1.) (-1.) (-1.),
        Tuple.vector (-1.) 0. 0. );
    ]
  in
  List.map
    (fun (name, point, normal) ->
      name >:: fun _ ->
      let res = Intersection.normal_at c point in
      assert_bool "is equal" (Tuple.is_equal normal res))
    testcases

let suite =
  "Cube tests"
  >::: [
         "Test create cube with defaults" >:: test_create_cube_defaults;
         "Test ray intersects cube" >::: test_ray_intersects_cube;
         "Test ray misses cube" >::: test_ray_misses_cube;
         "Test cube normal" >::: test_normal_at;
       ]

let () = run_test_tt_main suite
