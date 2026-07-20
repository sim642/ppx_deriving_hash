open OUnit2

open! Types (* force compilation *)
open! Giltho.Types (* force compilation *)

let tests =
  test_list [
    Alloc.tests;
    Giltho.Alloc.tests;
  ]

let () = run_test_tt_main tests
