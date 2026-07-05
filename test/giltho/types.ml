(* From https://github.com/sim642/ppx_deriving_hash/pull/7. *)

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

type 'a hash_consed = { node : 'a; tag : int }

let hash_hash_consed _ x = x.tag

type tree_kind = TLeaf of int | TBranch of tree * tree
and tree = tree_kind hash_consed [@@deriving hash]


type wrapped = { raw : (int[@hash fun x -> x + 1]) } [@@deriving hash]
type pvar = [ `A | `B of int | `C of string ] [@@deriving hash]
type inline = Inl of { x : int; y : string } | Plain [@@deriving hash]

type units = unit [@@deriving hash]
type i32 = int32 [@@deriving hash]
type i64 = int64 [@@deriving hash]

type ('a, 'b) two = Two of 'a * 'b [@@deriving hash]

type pairs = (int * int) list [@@deriving hash]
type rgb_arr = rgb array [@@deriving hash]

type ints = int list [@@deriving hash]
type 'a opt_alias = 'a option [@@deriving hash]

type wrapped_list = (int[@hash fun x -> x + 1]) list [@@deriving hash]
