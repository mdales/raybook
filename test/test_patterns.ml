open Raybook
open OUnit2

let test_solid_pattern_is_constant _ =
  let expected = Colour.v 0.2 0.3 0.4 in
  let p = Pattern.(v (Solid expected)) in
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 0. 0.));
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 1. 0.));
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 2. 0.));
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 0. 0.));
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 0. 1.));
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 0. 2.));
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 0. 0.));
  assert_equal expected (Pattern.colour_at p (Tuple.point 0.9 0. 0.));
  assert_equal expected (Pattern.colour_at p (Tuple.point 1. 0. 0.));
  assert_equal expected (Pattern.colour_at p (Tuple.point (-0.1) 0. 0.));
  assert_equal expected (Pattern.colour_at p (Tuple.point (-1.) 0. 0.));
  assert_equal expected (Pattern.colour_at p (Tuple.point (-1.1) 0. 0.))

let test_stripes_constant_in_y _ =
  let p = Pattern.(v (Stripes (Colour.white, Colour.black))) in
  let expected = Colour.white in
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 0. 0.));
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 1. 0.));
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 2. 0.))

let test_stripes_constant_in_z _ =
  let p = Pattern.(v (Stripes (Colour.white, Colour.black))) in
  let expected = Colour.white in
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 0. 0.));
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 0. 1.));
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 0. 2.))

let test_stripes_alternates_in_x _ =
  let p = Pattern.(v (Stripes (Colour.white, Colour.black))) in
  let w = Colour.white and b = Colour.black in
  assert_equal w (Pattern.colour_at p (Tuple.point 0. 0. 0.));
  assert_equal w (Pattern.colour_at p (Tuple.point 0.9 0. 0.));
  assert_equal b (Pattern.colour_at p (Tuple.point 1. 0. 0.));
  assert_equal b (Pattern.colour_at p (Tuple.point (-0.1) 0. 0.));
  assert_equal b (Pattern.colour_at p (Tuple.point (-1.) 0. 0.));
  assert_equal w (Pattern.colour_at p (Tuple.point (-1.1) 0. 0.))

let test_strips_with_object_translation _ =
  let pt = Tuple.point 1.5 0. 0. in
  let t = Transformation.scaling 2. 2. 2. in
  let s = Shape.(v ~transform:t Sphere) in
  let p = Pattern.(v (Stripes (Colour.white, Colour.black))) in

  let ptm = Tuple.to_matrix pt in
  let optm = Matrix.multiply (Shape.inverse_transform s) ptm in
  let opt = Tuple.of_matrix optm in

  let res = Pattern.colour_at p opt in
  assert_equal Colour.white res

let test_strips_with_pattern_translation _ =
  let t = Transformation.scaling 2. 2. 2. in
  let p = Pattern.(v ~transform:t (Stripes (Colour.white, Colour.black))) in
  let res = Pattern.colour_at p (Tuple.point 1.5 0. 0.) in
  assert_equal Colour.white res

let test_strips_with_object_and_pattern_translation _ =
  let pt = Tuple.point 2.5 0. 0. in
  let t1 = Transformation.scaling 2. 2. 2. in
  let s = Shape.(v ~transform:t1 Sphere) in

  let ptm = Tuple.to_matrix pt in
  let optm = Matrix.multiply (Shape.inverse_transform s) ptm in
  let opt = Tuple.of_matrix optm in

  let t2 = Transformation.translation 0.5 0. 0. in
  let p = Pattern.(v ~transform:t2 (Stripes (Colour.white, Colour.black))) in
  let res = Pattern.colour_at p opt in
  assert_equal Colour.white res

let test_gradient_pattern _ =
  let p = Pattern.(v (Gradient (Colour.white, Colour.black))) in
  let expected = Colour.white in
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 0. 0.));
  let expected = Colour.v 0.75 0.75 0.75 in
  assert_equal expected (Pattern.colour_at p (Tuple.point 0.25 0. 0.));
  let expected = Colour.v 0.5 0.5 0.5 in
  assert_equal expected (Pattern.colour_at p (Tuple.point 0.5 0. 0.));
  let expected = Colour.v 0.25 0.25 0.25 in
  assert_equal expected (Pattern.colour_at p (Tuple.point 0.75 0. 0.))

let test_ring_pattern _ =
  let p = Pattern.(v (Rings (Colour.white, Colour.black))) in
  let expected = Colour.white in
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 0. 0.));
  let expected = Colour.black in
  assert_equal expected (Pattern.colour_at p (Tuple.point 1. 0. 0.));
  assert_equal expected (Pattern.colour_at p (Tuple.point 1. 0. 1.));
  assert_equal expected (Pattern.colour_at p (Tuple.point 0.708 0. 0.708))

let suite =
  "Pattern tests"
  >::: [
         "Test solid pattern is constant" >:: test_solid_pattern_is_constant;
         "Test stripes constant in y" >:: test_stripes_constant_in_y;
         "Test stripes constant in z" >:: test_stripes_constant_in_z;
         "Test stripes alternatte in x" >:: test_stripes_alternates_in_x;
         "Test stripes with object translation"
         >:: test_strips_with_object_translation;
         "Test stripes with pattern translation"
         >:: test_strips_with_pattern_translation;
         "Test stripes with object and pattern translation"
         >:: test_strips_with_object_and_pattern_translation;
         "Test gradient pattern" >:: test_gradient_pattern;
         "Test ring pattern" >:: test_ring_pattern;
       ]

let () = run_test_tt_main suite
