open Raybook
open OUnit2

let test_intersect_with_empty_group _ =
  let g = Shape.(v (Group [])) in
  let r = Ray.v (Specialised.point 0. 0. 0.) (Specialised.vector 0. 0. 1.) in
  let il = Intersection.intersects g r in
  assert_equal [] il

let test_intersect_with_nonempty_group _ =
  let s1 = Shape.(v Sphere) in
  let t2 = Transformation.translation 0. 0. (-3.) in
  let s2 = Shape.(v ~transform:t2 Sphere) in
  let t3 = Transformation.translation 5. 0. 0. in
  let s3 = Shape.(v ~transform:t3 Sphere) in
  let g = Shape.(v (Group [ s1; s2; s3 ])) in
  let r = Ray.v (Specialised.point 0. 0. (-5.)) (Specialised.vector 0. 0. 1.) in
  let il = Intersection.intersects g r in
  assert_equal 4 (List.length il);
  assert_equal s2 (Intersection.shape (List.nth il 0));
  assert_equal s2 (Intersection.shape (List.nth il 1));
  assert_equal s1 (Intersection.shape (List.nth il 2));
  assert_equal s1 (Intersection.shape (List.nth il 3))

let test_intersect_transformed_group _ =
  let ts = Transformation.translation 5. 0. 0. in
  let s = Shape.(v ~transform:ts Sphere) in
  let tg = Transformation.scaling 2. 2. 2. in
  let g = Shape.(v ~transform:tg (Group [ s ])) in
  let r =
    Ray.v (Specialised.point 10. 0. (-10.)) (Specialised.vector 0. 0. 1.)
  in
  let il = Intersection.intersects g r in
  assert_equal 2 (List.length il)

let test_group_item_moved_to_world_space _ =
  let t = Transformation.translation 0. 1. 0. in
  let s = Shape.(v Sphere) in
  let g = Shape.(v ~transform:t (Group [ s ])) in

  let updated_s =
    match Shape.geometry g with
    | Group isl ->
        assert_equal 1 (List.length isl);
        List.hd isl
    | _ -> assert_failure "expected group"
  in
  let res = Shape.transform updated_s in
  assert_bool "is equal" (Specialised.is_equal t res);
  let v = Float.sqrt 2. /. 2. in
  let p = Specialised.point 0. (1. +. v) (0. -. v) in
  let res = Intersection.normal_at updated_s p in
  let expected = Specialised.vector 0. v (-1. *. v) in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_group_item_moved_to_world_space_both_transformed _ =
  (* Note the rotate on a sphere is nothing if done first *)
  let ts = Transformation.rotate_x (Float.pi /. 2.) in
  let tg = Transformation.translation 0. 1. 0. in
  let s = Shape.(v ~transform:ts Sphere) in
  let g = Shape.(v ~transform:tg (Group [ s ])) in

  let updated_s =
    match Shape.geometry g with
    | Group isl ->
        assert_equal 1 (List.length isl);
        List.hd isl
    | _ -> assert_failure "expected group"
  in
  let res = Shape.transform updated_s in
  let expected_t = Specialised.multiply tg ts in
  assert_bool "is equal" (Specialised.is_equal expected_t res);
  let v = Float.sqrt 2. /. 2. in
  let p = Specialised.point 0. (1. +. v) (0. -. v) in
  let res = Intersection.normal_at updated_s p in
  let expected = Specialised.vector 0. v (-1. *. v) in
  assert_bool "is equal" (Specialised.is_equal expected res)

let test_normal_at_within_group _ =
  let t = Transformation.translation 5. 0. 0. in
  let s = Shape.(v ~transform:t Sphere) in
  let t2 = Transformation.scaling 1. 2. 3. in
  let g2 = Shape.(v ~transform:t2 (Group [ s ])) in
  let t1 = Transformation.rotate_y (Float.pi /. 2.) in
  let g1 = Shape.(v ~transform:t1 (Group [ g2 ])) in

  let updated_s =
    match Shape.geometry g1 with
    | Group sl -> (
        assert_equal 1 (List.length sl);
        let inner_group = List.hd sl in
        match Shape.geometry inner_group with
        | Group isl ->
            assert_equal 1 (List.length isl);
            List.hd isl
        | _ -> assert_failure "expected inner group")
    | _ -> assert_failure "expected group"
  in
  let expected_transform = Transformation.combine [ t; t2; t1 ] in
  assert_bool "is equal transform"
    (Specialised.is_equal expected_transform (Shape.transform updated_s));
  let res =
    Intersection.normal_at updated_s (Specialised.point 1.7321 1.1547 (-5.5774))
  in
  let expected =
    Specialised.vector 0.28570368184140720880 0.42854315178114094076
      (-0.85716052944810172676)
  in
  assert_bool "is equal normal" (Specialised.is_equal expected res)

let suite =
  "Group tests"
  >::: [
         "Test intersects empty group" >:: test_intersect_with_empty_group;
         "Test intersects non empty group"
         >:: test_intersect_with_nonempty_group;
         "Test intersect transformed group" >:: test_intersect_transformed_group;
         "Test group updates inner item transform"
         >:: test_group_item_moved_to_world_space;
         "Test group updates inner item when both transformed"
         >:: test_group_item_moved_to_world_space_both_transformed;
         "Test normal at for shape in group" >:: test_normal_at_within_group;
       ]

let () = run_test_tt_main suite
