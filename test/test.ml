(* From https://github.com/sim642/ppx_deriving_hash/pull/7. *)
type 'a hash_consed = { node : 'a; tag : int }
let hash_hash_consed _ x = x.tag

type tree_kind = TLeaf of int | TBranch of tree * tree
and tree = tree_kind hash_consed [@@deriving hash]
