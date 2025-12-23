open Raybook
open OUnit2

let test_create_canvas _ =
  let res = Canvas.v (10, 20) in
  let width, height = Canvas.dimensions res in
  assert_equal 10 width;
  assert_equal 20 height;
  let expected = Colour.v 0. 0. 0. in
  for y = 0 to 19 do
    for x = 0 to 9 do
      let c = Canvas.read_pixel res (x, y) in
      assert_bool "is equal" (Colour.is_equal expected c)
    done
  done

let test_create_canvas_width_too_small _ =
  assert_raises (Invalid_argument "Invalid width") (fun () ->
      let _ = Canvas.v (-10, 20) in
      ())

let test_create_canvas_height_too_small _ =
  assert_raises (Invalid_argument "Invalid height") (fun () ->
      let _ = Canvas.v (10, -20) in
      ())

let test_write_pixel _ =
  let c = Canvas.v (10, 20) in
  let red = Colour.v 1. 0. 0. in
  Canvas.write_pixel c (2, 3) red;
  let c = Canvas.read_pixel c (2, 3) in
  assert_bool "is equal" (Colour.is_equal red c)

let suite =
  "Tuple tests"
  >::: [
         "Test create canvas" >:: test_create_canvas;
         "Test create canvas width too small"
         >:: test_create_canvas_width_too_small;
         "Test create canvas height too small"
         >:: test_create_canvas_height_too_small;
         "Test write pixel" >:: test_write_pixel;
       ]

let () = run_test_tt_main suite
