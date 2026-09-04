(* From https://github.com/sim642/ppx_deriving_hash/pull/7. *)
open OUnit2
open Test_util
open Types


let e = Add (Lit 1, Many [ Neg (Lit 2); Lab ("x", Lit 3) ])
let t = { node = TBranch ({ node = TLeaf 1; tag = 1 }, { node = TLeaf 2; tag = 2 }); tag = 3 }
let leaf i = { node = PLeaf i; tag = i }
let pt = PBranch (leaf 1, leaf 2)
let hash_pseq' = hash_pseq (fun x -> x) Char.code

let tests =
  "giltho" >::: [
    "alloc" >::: [
#if OCAML_VERSION < (5, 2, 0)
      "expr" >:: test_no_alloc hash_expr e;
#endif
      "nested" >:: test_no_alloc hash_nested [| [ [ 1; 2; 3 ]; [ 4; 5 ] ]; [ [ 6 ] ] |];
      "rgb" >:: test_no_alloc hash_rgb { r = 1; g = 2; b = 3 };
      "pair" >:: test_no_alloc hash_pair (4, 5);
      "tree" >:: test_no_alloc hash_tree t;
#if OCAML_VERSION < (5, 2, 0)
      "list" >:: test_no_alloc (hash_poly (fun x -> x)) (Cons (1, Cons (2, Cons (3, Nil))));
#endif
      (* primitives mixed (incl. a float field, which must not re-box on read) *)
      "prim" >:: test_no_alloc hash_prim { i = 1; b = true; c = 'a'; s = "x"; f = 3.5 };
      "option" >:: test_no_alloc hash_opt (Some 5);
      "ref" >:: test_no_alloc hash_refd (ref 7);
      "lazy" >:: test_no_alloc hash_lazyd (lazy 9);
      "poly variant" >:: test_no_alloc hash_pvar (`B 1);
      "inline record" >:: test_no_alloc hash_inline (Inl { x = 1; y = "a" });
      "two params" >:: test_no_alloc (hash_two (fun x -> x) Char.code) (Two (1, 'a'));
      "pairs" >:: test_no_alloc hash_pairs [ (1, 2); (3, 4); (5, 6) ];
      "rgb array" >:: test_no_alloc hash_rgb_arr [| { r = 1; g = 2; b = 3 } |];
      "override in list" >:: test_no_alloc hash_wrapped_list [ 10; 20; 30 ];
      "poly override" >:: test_no_alloc (hash_ptree_kind (fun x -> x) Char.code) pt;
      "poly alias" >:: test_no_alloc (hash_two_wrap (fun x -> x) Char.code) (Wrap (Two (1, 'a')));
      "poly list" >:: test_no_alloc hash_pseq' (PMany [ leaf 1; leaf 2; leaf 3 ]);
      "poly array" >:: test_no_alloc hash_pseq' (PArr [| leaf 1; leaf 2; leaf 3 |]);
      "poly tuple list" >:: test_no_alloc hash_pseq' (PPairs [ (1, 'a'); (2, 'b') ]);
    ];
  ]
