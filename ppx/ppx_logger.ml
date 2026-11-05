(* ppx_logger.ml *)
open Ppxlib
open Ast_helper

let is_logger_call exp =
  match exp.pexp_desc with
  | Pexp_ident { txt = Ldot (Lident "Logger", "debug"); _ } -> true
  | _ -> false

(* Helper to create literal syntax constants *)
let int_expr ~loc n = Exp.constant ~loc (Const.int n)
let string_expr ~loc s = Exp.constant ~loc (Const.string s)

(* Synthesizes a native literal Lexing.position record in the AST *)
let make_lexing_pos_ast ~loc (pos : Lexing.position) =
  let open Longident in
  Exp.record ~loc
    [
      ( { txt = Ldot (Lident "Lexing", "pos_fname"); loc },
        string_expr ~loc pos.pos_fname );
      ( { txt = Ldot (Lident "Lexing", "pos_lnum"); loc },
        int_expr ~loc pos.pos_lnum );
      ( { txt = Ldot (Lident "Lexing", "pos_bol"); loc },
        int_expr ~loc pos.pos_bol );
      ( { txt = Ldot (Lident "Lexing", "pos_cnum"); loc },
        int_expr ~loc pos.pos_cnum );
    ]
    None

let mappers =
  object
    inherit Ast_traverse.map as super

    method! expression exp =
      match exp.pexp_desc with
      | Pexp_apply (func, args) when is_logger_call func ->
          let loc = exp.pexp_loc in

          (* 1. Grab the precise compiler-level lexing positions *)
          let start_ast = make_lexing_pos_ast ~loc loc.loc_start in
          let end_ast = make_lexing_pos_ast ~loc loc.loc_end in

          (* 2. Bundle them together into a standard tuple pair: (start, end) *)
          let pair_ast = Exp.tuple ~loc [ start_ast; end_ast ] in

          (* 3. Inject `~pos_pair:(start_pos, end_pos)` into the arguments list *)
          let new_args = (Labelled "pos_pair", pair_ast) :: args in
          Exp.apply ~loc func new_args
      | _ -> super#expression exp
  end

let () = Driver.register_transformation "ppx_logger" ~impl:mappers#structure
