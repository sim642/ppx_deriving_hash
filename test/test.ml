open OUnit2

open! Types (* force compilation *)

let tests =
  test_list [
    Alloc.tests;
  ]

let () = run_test_tt_main tests