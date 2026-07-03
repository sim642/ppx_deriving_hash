open OUnit2
open Types

let measure_allocated_bytes f =
  let a0 = Gc.allocated_bytes () in
  let a1 = Gc.allocated_bytes () in
  ignore (Sys.opaque_identity (f ()));
  let a2 = Gc.allocated_bytes () in
  (a2 -. a1) -. (a1 -. a0)

let assert_no_alloc f =
  let allocated_bytes = measure_allocated_bytes f in
  assert_bool (Printf.sprintf "expected no allocation, but actually allocated %.0f bytes" allocated_bytes) (allocated_bytes < 1.0)

let test_no_alloc hash x _ctxt =
  assert_no_alloc (fun () -> hash x)


(* From https://github.com/sim642/ppx_deriving_hash/pull/7. *)

let tests =
  "alloc" >::: [
    "list" >:: test_no_alloc (hash_poly (fun x -> x)) (Cons (1, Cons (2, Cons (3, Nil))));
    "list2" >:: test_no_alloc (hash_poly2 (fun x -> x) (fun x -> Char.code x)) (Cons2 (1, 'a', Cons2 (2, 'b', Cons2 (3, 'c', Nil2))));
    "rgb array" >:: test_no_alloc hash_rgb_arr [| { r = 1; g = 2; b = 3 } |];
    "override in list" >:: test_no_alloc hash_wrapped_list [ 10; 20; 30 ];
  ]

