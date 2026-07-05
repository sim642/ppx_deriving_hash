open OUnit2

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
