(* Tests for [@@deriving hash]:
   - the various type shapes compile (notably a recursive type reached through a
     parametrized alias, which must be a valid [let rec] right-hand side);
   - [hash] is consistent with structural equality;
   - [hash] never allocates. *)

type flat = A | B | C [@@deriving hash]
type rgb = { r : int; g : int; b : int } [@@deriving hash]
type pair = int * int [@@deriving hash]
type prim = { i : int; b : bool; c : char; s : string; f : float } [@@deriving hash]

type expr =
  | Lit of int
  | Neg of expr
  | Add of expr * expr
  | Many of expr list
  | Lab of string * expr
[@@deriving hash]

type nested = int list list array [@@deriving hash]
type opt = int option [@@deriving hash]
type refd = int ref [@@deriving hash]
type lazyd = int lazy_t [@@deriving hash]
type 'a poly = Nil | Cons of 'a * 'a poly [@@deriving hash]

(* Recursive type via a parametrized alias (the hash-consing shape). *)
type 'a hash_consed = { node : 'a; tag : int }

let hash_hash_consed _ x = x.tag

type tree_kind = TLeaf of int | TBranch of tree * tree
and tree = tree_kind hash_consed [@@deriving hash]

(* Override via [@hash]. *)
type wrapped = { raw : (int[@hash fun x -> x + 1]) } [@@deriving hash]

let failures = ref 0

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    incr failures;
    Printf.printf "  FAIL %s\n" name)

(* Assert a hash function does not allocate, by hashing one value in a tight
   loop and checking the per-call allocation rounds to zero. *)
let check_no_alloc name hash v =
  let n = 1_000_000 in
  let acc = ref 0 in
  acc := !acc lxor hash v;
  (* warm up / no-op to stabilize *)
  let a0 = Gc.allocated_bytes () in
  for _ = 1 to n do
    acc := !acc lxor hash v
  done;
  let a1 = Gc.allocated_bytes () in
  ignore (Sys.opaque_identity !acc);
  let per_op = (a1 -. a0) /. float_of_int n in
  check (Printf.sprintf "%s allocates %.3f B/op (expected ~0)" name per_op)
    (per_op < 1.0)

let () =
  (* Determinism *)
  let e = Add (Lit 1, Many [ Neg (Lit 2); Lab ("x", Lit 3) ]) in
  check "deterministic" (hash_expr e = hash_expr e);

  (* Consistency with structural equality *)
  let e2 = Add (Lit 1, Many [ Neg (Lit 2); Lab ("x", Lit 3) ]) in
  check "equal values -> equal hash" (hash_expr e = hash_expr e2);
  check "rgb equal" (hash_rgb { r = 1; g = 2; b = 3 } = hash_rgb { r = 1; g = 2; b = 3 });
  check "pair equal" (hash_pair (4, 5) = hash_pair (4, 5));
  check "list equal" (hash_expr (Many [ Lit 1; Lit 2 ]) = hash_expr (Many [ Lit 1; Lit 2 ]));

  (* Distinct values usually hash differently (sanity, not guaranteed) *)
  check "distinct differ" (hash_expr (Lit 1) <> hash_expr (Lit 2));

  (* Compile/usage smoke for the trickier shapes *)
  check "nested compiles" (hash_nested [| [ [ 1; 2 ]; [ 3 ] ] |] = hash_nested [| [ [ 1; 2 ]; [ 3 ] ] |]);
  check "option" (hash_opt (Some 1) <> hash_opt None);
  check "ref" (hash_refd (ref 7) = hash_refd (ref 7));
  check "lazy" (hash_lazyd (lazy 9) = hash_lazyd (lazy 9));
  check "poly" (hash_poly (fun x -> x) (Cons (1, Nil)) = hash_poly (fun x -> x) (Cons (1, Nil)));
  let t = { node = TBranch ({ node = TLeaf 1; tag = 1 }, { node = TLeaf 2; tag = 2 }); tag = 3 } in
  check "tree via alias" (hash_tree t = t.tag);
  check "hash override applied" (hash_wrapped { raw = 10 } <> hash_wrapped { raw = 11 });

  (* Allocation-freedom *)
  check_no_alloc "expr" hash_expr e;
  check_no_alloc "nested" hash_nested [| [ [ 1; 2; 3 ]; [ 4; 5 ] ]; [ [ 6 ] ] |];
  check_no_alloc "rgb" hash_rgb { r = 1; g = 2; b = 3 };
  check_no_alloc "pair" hash_pair (4, 5);
  check_no_alloc "tree" hash_tree t;
  check_no_alloc "list" (hash_poly (fun x -> x)) (Cons (1, Cons (2, Cons (3, Nil))));

  if !failures = 0 then print_endline "All tests passed."
  else (
    Printf.printf "%d failure(s).\n" !failures;
    exit 1)
