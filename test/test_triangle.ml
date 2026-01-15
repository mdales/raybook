open Raybook
open OUnit2

let test_simple_triangle _ =
  let p1 = Tuple.point 0. 1. 0.
  and p2 = Tuple.point (-1.) 0. 0.
  and p3 = Tuple.point 1. 0. 0. in
  let res = Shape.(v (Triangle (p1, p2, p3))) in
  match Shape.edges res with
  | None -> assert_failure "expected edges"
  | Some (e1, e2) -> (
      (let expected_e1 = Tuple.vector (-1.) (-1.) 0.
       and expected_e2 = Tuple.vector 1. (-1.) 0. in
       assert_bool "is equal e1" (Tuple.is_equal expected_e1 e1);
       assert_bool "is equal e2" (Tuple.is_equal expected_e2 e2));
      match Shape.normal res with
      | None -> assert_failure "expected normal"
      | Some n ->
          let expected = Tuple.vector 0. 0. (-1.) in
          assert_bool "is equal n" (Tuple.is_equal expected n))

let test_triangle_normal_at =
  let p1 = Tuple.point 0. 1. 0.
  and p2 = Tuple.point (-1.) 0. 0.
  and p3 = Tuple.point 1. 0. 0. in
  let t = Shape.(v (Triangle (p1, p2, p3))) in
  let testcases =
    [
      ("test 1", Tuple.point 0. 0.5 0.);
      ("test 2", Tuple.point (-0.5) 0.75 0.);
      ("test 3", Tuple.point 0.25 0.75 0.);
    ]
  in
  let expected = Tuple.vector 0. 0. (-1.) in
  List.map
    (fun (name, point) ->
      name >:: fun _ ->
      let res = Intersection.normal_at t point in
      assert_bool "is equal" (Tuple.is_equal expected res))
    testcases

let test_ray_misses =
  let p1 = Tuple.point 0. 1. 0.
  and p2 = Tuple.point (-1.) 0. 0.
  and p3 = Tuple.point 1. 0. 0. in
  let t = Shape.(v (Triangle (p1, p2, p3))) in
  let testcases =
    [
      ("parallel", Ray.v (Tuple.point 0. (-1.) (-2.)) (Tuple.vector 0. 1. 0.));
      ( "between p1 and p3",
        Ray.v (Tuple.point 1. 1. (-2.)) (Tuple.vector 0. 0. 1.) );
      ( "between p1 and p2",
        Ray.v (Tuple.point (-1.) 1. (-2.)) (Tuple.vector 0. 0. 1.) );
      ( "between p2 and p3",
        Ray.v (Tuple.point 0. (-1.) (-2.)) (Tuple.vector 0. 0. 1.) );
    ]
  in
  List.map
    (fun (name, r) ->
      name >:: fun _ ->
      let res = Intersection.intersects t r in
      assert_equal res [])
    testcases

let test_ray_hits _ =
  let p1 = Tuple.point 0. 1. 0.
  and p2 = Tuple.point (-1.) 0. 0.
  and p3 = Tuple.point 1. 0. 0. in
  let t = Shape.(v (Triangle (p1, p2, p3))) in
  let r = Ray.v (Tuple.point 0. 0.5 (-2.)) (Tuple.vector 0. 0. 1.) in
  let res = Intersection.intersects t r in
  assert_equal 1 (List.length res);
  assert_equal 2. (Intersection.distance (List.hd res))

let suite =
  "Triangle tests"
  >::: [
         "Test simple triangle" >:: test_simple_triangle;
         "Test normal at" >::: test_triangle_normal_at;
         "Test ray does not intersect" >::: test_ray_misses;
         "Test ray hits" >:: test_ray_hits;
       ]

let () = run_test_tt_main suite
