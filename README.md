# ppx_deriving_hash
## `[@@deriving hash]`

Deriver for standard hash functions without extra dependencies.


## Installation
```console
opam install ppx_deriving_hash
```

## Usage
In dune:
```
(preprocess (pps ppx_deriving_hash))
```

## Syntax
* Use `[@@deriving hash]` after a type definition to derive the function `val hash: t -> int` for it (if the type is named `t`) or `val ty_hash: ty -> int` (otherwise if the type is named `ty`).
* Use `[@hash fun x -> ...]` after a type expression to override the underlying hash function used for it.
* Use `[%hash: ty]` as an expression for the hash function of type `ty`.
