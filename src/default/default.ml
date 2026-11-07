open Ppxlib
open Ast_builder.Default
open Ppx_deriving_hash_api


module DefaultAlgorithm: Algorithm =
struct
  type args = {
    multiplier: int;
    seed: int;
  }
  let default_args = {
    multiplier = 13;
    seed = 0;
  }

  type 'r with_args = int option -> int option -> 'r
  let args () = Deriving.Args.(
    empty
    +> arg "multiplier" (Ast_pattern.eint __)
    +> arg "seed" (Ast_pattern.eint __)
  )
  let parse_args f multiplier seed =
    let args = {
        multiplier = Option.value multiplier ~default:default_args.multiplier;
        seed = Option.value seed ~default:default_args.seed;
      }
    in
    f ~args

  let seed ~args ~loc = eint ~loc args.seed
  let mix ~args ~loc a b =
    [%expr [%e eint ~loc args.multiplier] * [%e a] + [%e b]]
end

module DefaultParam: Param =
struct
  let name = "hash"
  include DefaultAlgorithm
end

module DefaultDeriver = Make (DefaultParam)
