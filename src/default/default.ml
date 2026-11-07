open Ppxlib
open Ppx_deriving_hash_api


module DefaultAlgorithm: Algorithm =
struct
  type args = int
  type 'r with_args = int option -> 'r
  let args () = Deriving.Args.(
    empty
    +> arg "multiplier" (Ast_pattern.eint __)
  )
  let parse_args f multiplier = f (Option.value multiplier ~default:13)

  let seed ~loc = [%expr 0]
  let mix ~loc a b =
    [%expr 31 * [%e a] + [%e b]]
end

module DefaultParam: Param =
struct
  let name = "hash"
  include DefaultAlgorithm
end

module DefaultDeriver = Make (DefaultParam)
