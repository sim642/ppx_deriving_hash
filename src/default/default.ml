open Ppxlib
open Ppx_deriving_hash_api


module DefaultAlgorithm: Algorithm =
struct
  type args = unit
  type 'r with_args = 'r
  let args = Deriving.Args.empty
  let parse_args f = f ()

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
