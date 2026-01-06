open Raybook
open OUnit2

let almost_equal a b =
  assert_bool
    (Printf.sprintf "%f vs %f" a b)
    (Float.abs (a -. b) < Float.epsilon)

let glass_sphere ?(refactive_index = 1.5) t =
  let p = Pattern.(v (Solid Colour.white)) in
  let m =
    Material.v ~pattern:p ~transparency:1.0 ~refractive_index:refactive_index ()
  in
  Shape.(v ~material:m ~transform:t Sphere)

let test_create_intersection _ =
  let s = Shape.(v Sphere) in
  let res = Intersection.v s 3.5 in
  assert_equal 3.5 (Intersection.distance res);
  assert_equal s (Intersection.shape res)

let test_hit_all_positive _ =
  let s = Shape.(v Sphere) in
  let tl = [ Intersection.v s 1.; Intersection.v s 2. ] in
  let res = Intersection.hit tl in
  match res with
  | None -> assert_bool "unexpected" false
  | Some i -> assert_equal 1. (Intersection.distance i)

let test_hit_mixed_positive_negative _ =
  let s = Shape.(v Sphere) in
  let tl = [ Intersection.v s (-1.); Intersection.v s 1. ] in
  let res = Intersection.hit tl in
  match res with
  | None -> assert_bool "unexpected" false
  | Some i -> assert_equal 1. (Intersection.distance i)

let test_hit_all_negative _ =
  let s = Shape.v Shape.Sphere in
  let tl = [ Intersection.v s (-2.); Intersection.v s (-1.) ] in
  let res = Intersection.hit tl in
  match res with None -> () | Some _ -> assert_bool "unexpected" false

let test_mixed_order_mixed_sign _ =
  let s = Shape.(v Sphere) in
  let tl =
    [
      Intersection.v s 5.;
      Intersection.v s 7.;
      Intersection.v s (-3.);
      Intersection.v s 2.;
    ]
  in
  let res = Intersection.hit tl in
  match res with
  | None -> assert_bool "unexpected" false
  | Some i -> assert_equal 2. (Intersection.distance i)

let test_hit_should_offset _ =
  let r = Ray.v (Tuple.point 0. 0. (-5.)) (Tuple.vector 0. 0. 1.) in
  let t = Transformation.translation 0. 0. 1. in
  let s = Shape.(v ~transform:t Sphere) in
  let i = Intersection.v s 5. in
  let comps = Precomputed.v i r [ i ] in
  let point = Precomputed.point comps in
  let over_point = Precomputed.over_point comps in
  assert_bool "is small" (Tuple.z over_point < Float.epsilon /. 2.);
  assert_bool "is greater" (Tuple.z point > Tuple.z over_point)

let test_reflect_ray _ =
  let x = Float.sqrt 2. /. 2. in
  let r = Ray.v (Tuple.point 0. 1. (-1.)) (Tuple.vector 0. (0. -. x) x) in
  let s = Shape.(v Plane) in
  let i = Intersection.v s x in
  let comps = Precomputed.v i r [ i ] in
  let res = Precomputed.reflectv comps in
  let expected = Tuple.vector 0. x x in
  assert_bool "is equal" (Tuple.is_equal expected res)

let test_n1_n2_intersections _ =
  let t1 = Transformation.scaling 2. 2. 2. in
  let a = glass_sphere t1 in
  let t2 = Transformation.translation 0. 0. 0.25 in
  let c = glass_sphere ~refactive_index:2.5 t2 in
  let t3 = Transformation.translation 0. 0. (-0.25) in
  let b = glass_sphere ~refactive_index:2. t3 in
  let r = Ray.v (Tuple.point 0. 0. (-4.)) (Tuple.vector 0. 0. 1.) in
  let il =
    [
      Intersection.v a 2.;
      Intersection.v b 2.75;
      Intersection.v c 3.25;
      Intersection.v b 4.75;
      Intersection.v c 5.25;
      Intersection.v a 6.;
    ]
  in
  let expected_n1_n2 =
    [ (1., 1.5); (1.5, 2.); (2., 2.5); (2.5, 2.5); (2.5, 1.5); (1.5, 1.) ]
  in
  let testdata = List.combine il expected_n1_n2 in
  List.iter
    (fun (i, (n1, n2)) ->
      let comp = Precomputed.v i r il in
      let res_n1, res_n2 = Precomputed.n_pair comp in
      almost_equal n1 res_n1;
      almost_equal n2 res_n2)
    testdata

let suite =
  "Intersection tests"
  >::: [
         "Test create intersection" >:: test_create_intersection;
         "Test hit all positive" >:: test_hit_all_positive;
         "Test hit mixed" >:: test_hit_mixed_positive_negative;
         "Test hit all negative" >:: test_hit_all_negative;
         "Test mixed order" >:: test_mixed_order_mixed_sign;
         "Test over point" >:: test_hit_should_offset;
         "Test reflect vector" >:: test_reflect_ray;
         "Test n1 n2 intersection" >:: test_n1_n2_intersections;
       ]

let () = run_test_tt_main suite
