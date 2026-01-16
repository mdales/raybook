open Raybook
open OUnit2

let test_chunk_empty_list _ =
    let res = X3d.chunks 3 [] in
    match res with
    | Result.Ok r -> assert_equal [] r
    | Result.Error _ -> assert_failure "Expected empty list"

let test_chunk_valid_list _ =
    let res = X3d.chunks 3 [1 ; 2 ;3 ;4 ;5 ;6] in
    match res with
    | Result.Ok r -> assert_equal [[1 ;2 ;3 ]; [4; 5; 6]] r
    | Result.Error _ -> assert_failure "Expected valid list"

let test_chunk_invalid_list _ =
    let res = X3d.chunks 3 [1 ; 2 ;3 ;4 ;5 ] in
    match res with
    | Result.Ok _ -> assert_failure "Expected failure"
    | Result.Error _ -> ()

let suite =
  "X3D tests"
  >::: [
    "Test chunk empy list" >:: test_chunk_empty_list;
    "Test chunk valid list" >:: test_chunk_valid_list;
    "Test chunk with invalid list" >:: test_chunk_invalid_list;
       ]

let () = run_test_tt_main suite
