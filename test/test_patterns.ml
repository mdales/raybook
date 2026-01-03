open Raybook
open OUnit2

let test_create_stripe_pattern _ =
    let res = Pattern.v Pattern.Stripes (Colour.black, Colour.white) in
    assert_equal Pattern.Stripes (Pattern.style res);
    let expected_colours = (Colour.black, Colour.white) in
    assert_equal expected_colours (Pattern.colours res)


let suite =
  "Pattern tests"
  >::: [
    "Test create stripe pattern" >:: test_create_stripe_pattern;
  ]

let () = run_test_tt_main suite
