(* From https://github.com/sim642/ppx_deriving_hash/pull/7. *)

type rgb = { r : int; g : int; b : int } [@@deriving hash]
type 'a poly = Nil | Cons of 'a * 'a poly [@@deriving hash]
type ('a, 'b) poly2 = Nil2 | Cons2 of 'a * 'b * ('a, 'b) poly2 [@@deriving hash]

type 'a hash_consed = { node : 'a; tag : int }
let hash_hash_consed _ x = x.tag

type tree_kind = TLeaf of int | TBranch of tree * tree
and tree = tree_kind hash_consed [@@deriving hash]

type rgb_arr = rgb array [@@deriving hash]

type wrapped_list = (int[@hash fun x -> x + 1]) list [@@deriving hash]
