open Raybook
open OUnit2

let test_create_colour _ =
    let res = Colour.v 0.1 0.2 0.3 in
        assert_equal 0.1 (Colour.red res);
        assert_equal 0.2 (Colour.green res);
        assert_equal 0.3 (Colour.blue res)

let suite =
  "Tuple tests"
  >::: [
         "Test create colour" >:: test_create_colour;
]

let () = run_test_tt_main suite
