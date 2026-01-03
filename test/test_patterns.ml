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

let suite =
  "Pattern tests"
  >::: [
         "Test solid pattern is constant" >:: test_solid_pattern_is_constant;
         "Test stripes constant in y" >:: test_stripes_constant_in_y;
         "Test stripes constant in z" >:: test_stripes_constant_in_z;
         "Test stripes alternatte in x" >:: test_stripes_alternates_in_x;
       ]

let () = run_test_tt_main suite
