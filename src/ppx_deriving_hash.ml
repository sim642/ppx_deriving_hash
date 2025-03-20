open Ppxlib
open Ast_builder.Default

let mangle_affix = `Prefix "hash"

let attr_hash = Attribute.declare "deriving.hash.hash" Attribute.Context.core_type Ast_pattern.(single_expr_payload __) (fun expr -> expr)


let hash_reduce2 ~loc a b =
  [%expr 31 * [%e a] + [%e b]]

let hash_fold ~loc i =
  List.fold_left (hash_reduce2 ~loc) i

let hash_empty ~loc = [%expr 0]

let hash_reduce ~loc = function
  | [] -> hash_empty ~loc
  | [x] -> x
  | x :: xs -> hash_fold ~loc x xs (* omits hash_empty *)

let hash_variant ~loc i = eint ~loc i

let rec expr ~loc ~quoter ct =
  match Attribute.get attr_hash ct with
  | Some hash ->
    Ppx_deriving.quote ~quoter hash
  | None ->
    let expr = expr ~quoter in
    match ct with
    | [%type: float]
    | [%type: string] ->
      [%expr Hashtbl.hash]
    | [%type: char] ->
      [%expr Char.code]
    | [%type: bool] ->
      [%expr Bool.to_int]
    | [%type: int32] ->
      [%expr Int32.to_int]
    | [%type: int64] ->
      [%expr Int64.to_int]
    | [%type: int] ->
      [%expr fun (x: int) -> x]
    | [%type: unit] ->
      [%expr fun () -> [%e hash_empty ~loc]]
    | [%type: [%t? a] ref] ->
      [%expr fun x -> [%e expr ~loc a] !x]
    | [%type: [%t? a] option] ->
      [%expr function
        (* like variants *)
        | None -> [%e hash_variant ~loc 0]
        | Some x -> [%e hash_reduce2 ~loc (hash_variant ~loc 1) [%expr [%e expr ~loc a] x]]
      ]
    | [%type: [%t? a] list] ->
      [%expr List.fold_left (fun a b -> [%e hash_reduce2 ~loc [%expr a] [%expr [%e expr ~loc a] b]]) [%e hash_empty ~loc]]
    | [%type: [%t? a] array] ->
      [%expr Array.fold_left (fun a b -> [%e hash_reduce2 ~loc [%expr a] [%expr [%e expr ~loc a] b]]) [%e hash_empty ~loc]]
    | [%type: [%t? a] lazy_t]
    | [%type: [%t? a] Lazy.t] ->
      [%expr fun (lazy x) -> [%e expr ~loc a] x]
    | {ptyp_desc = Ptyp_constr ({txt = lid; loc}, args); _} ->
      let ident = pexp_ident ~loc {loc; txt = Ppx_deriving.mangle_lid mangle_affix lid} in
      let ident = Ppx_deriving.quote ~quoter ident in
      let apply_args =
        args
        |> List.map (fun ct ->
            (Nolabel, expr ~loc ct)
          )
      in
      pexp_apply ~loc ident apply_args
    | {ptyp_desc = Ptyp_tuple comps; _} ->
      expr_tuple ~loc ~quoter comps
    | {ptyp_desc = Ptyp_variant (rows, Closed, None); _} ->
      expr_poly_variant ~loc ~quoter rows
    | {ptyp_desc = Ptyp_var name; _} ->
      evar ~loc ("poly_" ^ name)
    | _ ->
      Location.raise_errorf ~loc "other"

and expr_poly_variant ~loc ~quoter rows =
  rows
  |> List.map (fun {prf_desc; _} ->
      match prf_desc with
      | Rtag ({txt = label; loc}, true, []) ->
        let variant_i = Ppx_deriving.hash_variant label in
        let variant_const = hash_variant ~loc variant_i in
        case ~lhs:(ppat_variant ~loc label None)
          ~guard:None
          ~rhs:variant_const
      | Rtag ({txt = label; loc}, false, [ct]) ->
        let variant_i = Ppx_deriving.hash_variant label in
        let variant_const = hash_variant ~loc variant_i in
        let label_fun = expr ~loc ~quoter ct in
        case ~lhs:(ppat_variant ~loc label (Some [%pat? x]))
          ~guard:None
          ~rhs:(hash_reduce2 ~loc variant_const [%expr [%e label_fun] x])
      | _ ->
        Location.raise_errorf ~loc "other variant"
    )
  |> pexp_function_cases ~loc

and expr_variant ~loc ~quoter constrs =
  constrs
  |> List.mapi (fun variant_i {pcd_name = {txt = label; loc}; pcd_args; pcd_res; _} ->
      let variant_const = hash_variant ~loc variant_i in
      match pcd_res, pcd_args with
      | None, Pcstr_tuple [] ->
        case ~lhs:(ppat_construct ~loc {loc; txt = Lident label} None)
          ~guard:None
          ~rhs:variant_const
      | None, Pcstr_tuple cts ->
        let label_field ~loc prefix i =
          let name = prefix ^ string_of_int i in
          pexp_ident ~loc {loc; txt = Lident name}
        in
        let body =
          cts
          |> List.mapi (fun i comp_type ->
              (i, expr ~loc ~quoter comp_type)
            )
          |> List.map (fun (i, label_fun) ->
              [%expr [%e label_fun] [%e label_field ~loc "x" i]]
            )
          |> hash_fold ~loc variant_const
        in
        let pat prefix =
          cts
          |> List.mapi (fun i _ ->
              let name = prefix ^ string_of_int i in
              ppat_var ~loc {loc; txt = name}
            )
          |> ppat_tuple ~loc
          |> fun x -> ppat_construct ~loc {loc; txt = Lident label} (Some x)
        in
        case ~lhs:(pat "x")
          ~guard:None
          ~rhs:body
      | None, Pcstr_record lds ->
        let label_field ~loc record_expr label =
          pexp_field ~loc record_expr {loc; txt = Lident label}
        in
        let body x_expr =
          lds
          |> List.map (fun {pld_name = {txt = label; loc}; pld_type; _} ->
              (label, expr ~loc ~quoter pld_type)
            )
          |> List.map (fun (label, label_fun) ->
              [%expr [%e label_fun] [%e label_field ~loc x_expr label]]
            )
          |> hash_fold ~loc variant_const
        in
        let pat = ppat_construct ~loc {loc; txt = Lident label} (Some [%pat? x]) in
        case ~lhs:pat
          ~guard:None
          ~rhs:(body [%expr x])
      | _ ->
        Location.raise_errorf ~loc "other variant"
    )
  |> pexp_function_cases ~loc

and expr_record ~loc ~quoter lds =
  let label_field ~loc record_expr label =
    pexp_field ~loc record_expr {loc; txt = Lident label}
  in
  let body x_expr =
    lds
    |> List.map (fun {pld_name = {txt = label; loc}; pld_type; _} ->
        (label, expr ~loc ~quoter pld_type)
      )
    |> List.map (fun (label, label_fun) ->
        [%expr [%e label_fun] [%e label_field ~loc x_expr label]]
      )
    |> hash_reduce ~loc
  in
  [%expr fun x -> [%e body [%expr x]]]

and expr_tuple ~loc ~quoter comps =
  let label_field ~loc prefix i =
    let name = prefix ^ string_of_int i in
    pexp_ident ~loc {loc; txt = Lident name}
  in
  let body =
    comps
    |> List.mapi (fun i comp_type ->
        (i, expr ~loc ~quoter comp_type)
      )
    |> List.map (fun (i, label_fun) ->
        [%expr [%e label_fun] [%e label_field ~loc "x" i]]
      )
    |> hash_reduce ~loc
  in
  let pat prefix =
    comps
    |> List.mapi (fun i _ ->
        let name = prefix ^ string_of_int i in
        ppat_var ~loc {loc; txt = name}
      )
    |> ppat_tuple ~loc
  in
  [%expr fun [%p pat "x"] -> [%e body]]

let expr_declaration ~loc ~quoter td = match td with
  | {ptype_kind = Ptype_abstract; ptype_manifest = Some ct; _} ->
    expr ~loc ~quoter ct
  | {ptype_kind = Ptype_abstract; _} ->
    Location.raise_errorf ~loc "Cannot derive accessors for abstract types"
  | {ptype_kind = Ptype_variant constrs; _} ->
    expr_variant ~loc ~quoter constrs
  | {ptype_kind = Ptype_open; _} ->
    Location.raise_errorf ~loc "Cannot derive accessors for open types"
  | {ptype_kind = Ptype_record fields; _} ->
    expr_record ~loc ~quoter fields

let typ ~loc td =
  let ct = Ppx_deriving.core_type_of_type_decl td in
  Ppx_deriving.poly_arrow_of_type_decl
    (fun param -> [%type: [%t param] -> int])
    td
    [%type: [%t ct] -> int]

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  type_declarations
  |> List.map (fun td ->
      let quoter = Ppx_deriving.create_quoter () in
      let expr = expr_declaration ~loc ~quoter td in
      let expr = Ppx_deriving.sanitize ~quoter expr in
      let expr = Ppx_deriving.poly_fun_of_type_decl td expr in
      let ct = typ ~loc td in
      let pat = ppat_var ~loc {loc; txt = Ppx_deriving.mangle_type_decl mangle_affix td} in
      let pat = ppat_constraint ~loc pat ct in
      Ast_helper.Vb.mk ~loc ~attrs:[Ppx_deriving.attr_warning [%expr "-39"]] pat expr
    )
  |> Ast_helper.Str.value ~loc Recursive
  |> fun v -> [v]

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

let generate_intf ~ctxt (_rec_flag, type_declarations): signature_item list =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  type_declarations
  |> List.map (fun td ->
      let ct = typ ~loc td in
      Ast_helper.Sig.value ~loc (Ast_helper.Val.mk {loc; txt = Ppx_deriving.mangle_type_decl mangle_affix td} ct)
    )

let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let extension ~loc ~path:_ ct =
  let quoter = Ppx_deriving.create_quoter () in
  Ppx_deriving.sanitize ~quoter (expr ~loc ~quoter ct)

let my_deriver =
  Deriving.add
    "hash"
    ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
    ~extension
