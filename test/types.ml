(* Extended from https://github.com/sim642/ppx_deriving_hash/pull/7. *)

(* To check syntactic arity with two type parameters. *)
type ('a, 'b) poly2 = Nil2 | Cons2 of 'a * 'b * ('a, 'b) poly2 [@@deriving hash]


(* From Goblint. *)

(* Generating a [function] for this and eta-expanding, type-directed constructor disambiguation no longer works. *)
type t =
  | Error
  | Warning
  | Safe
[@@deriving hash]
