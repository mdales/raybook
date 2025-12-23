open Raybook
open OUnit2

let test_create_colour _ =
    let res = Colour.v (-0.5) 0.4 1.7 in
    assert_equal (-0.5) (Colour.red res);
    assert_equal 0.4 (Colour.green res);
    assert_equal 1.7 (Colour.blue res)

let test_add_colours _ =
    let a1 = Colour.v 0.9 0.6 0.75
    and a2 = Colour.v 0.7 0.1 0.25 in
    let res = Colour.add a1 a2 in
    let expected = Colour.v 1.6 0.7 1.0 in
    assert_bool "is equal" (Colour.is_equal expected res)

let test_subract_colours _ =
    let a1 = Colour.v 0.9 0.6 0.75
    and a2 = Colour.v 0.7 0.1 0.25 in
    let res = Colour.subtract a1 a2 in
    let expected = Colour.v 0.2 0.5 0.5 in
    assert_bool "is equal" (Colour.is_equal expected res)

let test_multiply_colour_by_value  _ =
    let a1 = Colour.v 0.2 0.3 0.4 in
    let res = Colour.fmultiply a1 2. in
    let expected = Colour.v 0.4 0.6 0.8 in
    assert_bool "is equal" (Colour.is_equal expected res)

let test_multiply_colours _ =
    let a1 = Colour.v 1. 0.2 0.4
    and a2 = Colour.v 0.9 1. 0.1 in
    let res = Colour.multiply a1 a2 in
    let expected = Colour.v 0.9 0.2 0.04 in
    assert_bool "is equal" (Colour.is_equal expected res)

let suite =
  "Tuple tests"
  >::: [
         "Test create colour" >:: test_create_colour;
         "Test add colours" >:: test_add_colours;
         "Test subtract colours" >:: test_subract_colours;
         "Test multiply colour by value" >:: test_multiply_colour_by_value;
         "Test multiply colours" >:: test_multiply_colours;
]

let () = run_test_tt_main suite
