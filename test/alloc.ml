open OUnit2
open Test_util
open Types


let tests =
  "alloc" >::: [
#if OCAML_VERSION < (5, 2, 0)
    "list2" >:: test_no_alloc (hash_poly2 (fun x -> x) (fun x -> Char.code x)) (Cons2 (1, 'a', Cons2 (2, 'b', Cons2 (3, 'c', Nil2))));
#endif
  ]
