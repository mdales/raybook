open Raybook
open OUnit2

let test_create_stripe_pattern _ =
  let res = Pattern.v Pattern.Stripes (Colour.black, Colour.white) in
  assert_equal Pattern.Stripes (Pattern.style res);
  let expected_colours = (Colour.black, Colour.white) in
  assert_equal expected_colours (Pattern.colours res)

let test_stripes_constant_in_y _ =
  let p = Pattern.v Pattern.Stripes (Colour.black, Colour.white) in
  let expected = Colour.white in
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 0. 0.));
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 1. 0.));
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 2. 0.))

let test_stripes_constant_in_z _ =
  let p = Pattern.v Pattern.Stripes (Colour.black, Colour.white) in
  let expected = Colour.white in
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 0. 0.));
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 0. 1.));
  assert_equal expected (Pattern.colour_at p (Tuple.point 0. 0. 2.))

let test_stripes_alternates_in_x _ =
  let p = Pattern.v Pattern.Stripes (Colour.black, Colour.white) in
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
         "Test create stripe pattern" >:: test_create_stripe_pattern;
         "Test stripes constant in y" >:: test_stripes_constant_in_y;
         "Test stripes constant in z" >:: test_stripes_constant_in_z;
         "Test stripes alternatte in x" >:: test_stripes_alternates_in_x;
       ]

let () = run_test_tt_main suite
