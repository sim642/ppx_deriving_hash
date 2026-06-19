(* Benchmark for [@@deriving hash]: measures throughput and *per-call
   allocation* of the derived hash functions, and compares against the
   polymorphic [Stdlib.Hashtbl.hash] baseline.

   The derived hash should never allocate. *)

(* ---- Representative types ---- *)

type expr =
  | Lit of int
  | Neg of expr
  | Add of expr * expr
  | Many of expr list
  | Lab of string * expr
[@@deriving hash]

type rgb = { r : int; g : int; b : int } [@@deriving hash]
type pair = int * int [@@deriving hash]
type flat = A | B | C | D | E [@@deriving hash]
type ints = int list [@@deriving hash]
type matrix = int list list [@@deriving hash]

(* Recursive type reached through a parametrized alias ([_ hash_consed]) -- the
   shape used by hash-consing libraries. Exercises the eta-expanded application
   path (which must stay allocation-free and be a valid [let rec] RHS). *)
type 'a hash_consed = { node : 'a; tag : int }

let hash_hash_consed _ x = x.tag

type tree_kind = Leaf of int | Branch of tree * tree
and tree = tree_kind hash_consed [@@deriving hash]

(* ---- Value generators (deterministic) ---- *)

let () = Random.init 0x1234

let rec gen_expr depth =
  if depth <= 0 then
    match Random.int 2 with
    | 0 -> Lit (Random.int 1_000_000)
    | _ -> Lab (string_of_int (Random.int 1000), Lit (Random.int 100))
  else
    match Random.int 5 with
    | 0 -> Lit (Random.int 1_000_000)
    | 1 -> Neg (gen_expr (depth - 1))
    | 2 -> Add (gen_expr (depth - 1), gen_expr (depth - 1))
    | 3 -> Many (List.init (Random.int 5) (fun _ -> gen_expr (depth - 1)))
    | _ -> Lab (string_of_int (Random.int 1000), gen_expr (depth - 1))

let tag_counter = ref 0

let hc node =
  incr tag_counter;
  { node; tag = !tag_counter }

let rec gen_tree depth =
  if depth <= 0 then hc (Leaf (Random.int 1_000_000))
  else
    match Random.int 2 with
    | 0 -> hc (Leaf (Random.int 1_000_000))
    | _ -> hc (Branch (gen_tree (depth - 1), gen_tree (depth - 1)))

let sample n f = Array.init n (fun _ -> f ())

(* ---- Measurement ---- *)

let measure name ~reps arr hash =
  let n = Array.length arr in
  (* warmup *)
  let acc = ref 0 in
  for i = 0 to n - 1 do
    acc := !acc lxor hash (Array.unsafe_get arr i)
  done;
  Gc.full_major ();
  let a0 = Gc.allocated_bytes () in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to reps do
    for i = 0 to n - 1 do
      acc := !acc lxor hash (Array.unsafe_get arr i)
    done
  done;
  let t1 = Unix.gettimeofday () in
  let a1 = Gc.allocated_bytes () in
  ignore (Sys.opaque_identity !acc);
  let total = float_of_int (reps * n) in
  Printf.printf "  %-24s %7.2f ns/op   %6.1f B/op\n%!" name
    ((t1 -. t0) *. 1e9 /. total)
    ((a1 -. a0) /. total)

let group title arr ~reps derived =
  Printf.printf "%s (n=%d)\n" title (Array.length arr);
  measure "derived hash" ~reps arr derived;
  measure "Stdlib.Hashtbl.hash" ~reps arr Hashtbl.hash;
  print_newline ()

let () =
  group "expr (recursive, lists)"
    (sample 100_000 (fun () -> gen_expr 6))
    ~reps:50 hash_expr;
  group "rgb (record)"
    (sample 200_000 (fun () ->
         { r = Random.int 256; g = Random.int 256; b = Random.int 256 }))
    ~reps:200 hash_rgb;
  group "pair (tuple)"
    (sample 200_000 (fun () -> (Random.int 100000, Random.int 100000)))
    ~reps:200 hash_pair;
  group "flat (enum)"
    (sample 200_000 (fun () ->
         [| A; B; C; D; E |].(Random.int 5)))
    ~reps:200 hash_flat;
  group "ints (int list)"
    (sample 100_000 (fun () -> List.init (Random.int 16) (fun _ -> Random.int 1000)))
    ~reps:100 hash_ints;
  group "matrix (int list list)"
    (sample 50_000 (fun () ->
         List.init (Random.int 8) (fun _ ->
             List.init (Random.int 8) (fun _ -> Random.int 1000))))
    ~reps:50 hash_matrix;
  group "tree (recursive via alias)"
    (sample 100_000 (fun () -> gen_tree 6))
    ~reps:50 hash_tree
