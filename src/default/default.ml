open Ppxlib
open Ast_builder.Default
open Ppx_deriving_hash_api


module DefaultAlgorithm: Algorithm =
struct
  type args = int
  let default_args = 13

  type 'r with_args = int option -> 'r
  let args () = Deriving.Args.(
    empty
    +> arg "multiplier" (Ast_pattern.eint __)
  )
  let parse_args f multiplier = f ~args:(Option.value multiplier ~default:default_args)

  let seed ~args ~loc = [%expr 0]
  let mix ~args ~loc a b =
    [%expr [%e eint ~loc args] * [%e a] + [%e b]]
end

module DefaultParam: Param =
struct
  let name = "hash"
  include DefaultAlgorithm
end

module DefaultDeriver = Make (DefaultParam)
