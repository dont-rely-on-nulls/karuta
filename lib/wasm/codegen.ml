(* Lowers the Erlang abstract forms emitted for the BEAM into a WebAssembly
   text module linked against runtime/karuta.wat. Karuta only emits a small
   subset of Erlang: predicates are functions from terms to goals built out of
   karuta:conj/disj/call-with-fresh/query-variable and the builtins. *)

open Beam.Builder
module Form = Shared.Compiler.Form

let fail message =
  Logger.simply_error ("WebAssembly target: " ^ message);
  exit 1

type program = {
  atoms : int BatMap.String.t;
  functions : (string * string * int, string) Hashtbl.t;
  lambdas : Buffer.t;
  mutable lambda_names : string list;
}

let rec atoms_of_expr acc : Expr.t -> BatSet.String.t = function
  | Literal (Atom a) -> BatSet.String.add a acc
  | Cons (h, t) -> atoms_of_expr (atoms_of_expr acc h) t
  | Tuple ts -> List.fold_left atoms_of_expr acc ts
  | Fun (Call (f, args)) -> List.fold_left atoms_of_expr acc (f :: args)
  | Fun (CallWithModule (m, f, args)) ->
      List.fold_left atoms_of_expr acc (m :: f :: args)
  | Fun (Lambda clauses) ->
      List.fold_left
        (fun acc { Clause.body; _ } -> List.fold_left atoms_of_expr acc body)
        acc clauses
  | _ -> acc

let atom { atoms; _ } name = BatMap.String.find name atoms

(* The innermost binding of a variable wins *)
let index scope var =
  let rec go i found = function
    | [] -> found
    | v :: vs -> go (i + 1) (if v = var then Some i else found) vs
  in
  match go 0 None scope with
  | Some i -> i
  | None -> fail ("unbound variable " ^ var)

let rec term program scope : Expr.t -> string = function
  | Variable v ->
      Printf.sprintf "(array.get $Env (local.get $env) (i32.const %d))"
        (index scope v)
  | Literal (Atom a) ->
      Printf.sprintf "(ref.i31 (i32.const %d))" (atom program a)
  | Literal (Integer i) -> Printf.sprintf "(struct.new $Int (i64.const %d))" i
  | Literal Nil -> "(global.get $nil)"
  | Cons (h, t) ->
      Printf.sprintf "(struct.new $Cons %s %s)" (term program scope h)
        (term program scope t)
  | Tuple (Literal (Atom name) :: args) ->
      Printf.sprintf "(struct.new $Fun (i32.const %d) %s)" (atom program name)
        (env program scope args)
  | _ -> fail "unsupported term"

and env program scope args =
  Printf.sprintf "(array.new_fixed $Env %d%s)" (List.length args)
    (String.concat "" @@ List.map (fun a -> " " ^ term program scope a) args)

let rec list_elements : Expr.t -> Expr.t list = function
  | Literal Nil -> []
  | Cons (h, t) -> h :: list_elements t
  | _ -> fail "expected a list of goals"

let predicate_header name =
  Printf.sprintf
    "(func %s (type $Pred) (param $env (ref $Env)) (result (ref $Goal))\n\
    \  %s)\n"
    name

let rec goal program modl scope : Expr.t -> string = function
  | Fun (CallWithModule (Literal (Atom "karuta"), Literal (Atom f), args)) ->
      builtin program modl scope f args
  | Fun (CallWithModule (Literal (Atom m), Literal (Atom f), args)) ->
      call program scope m f args
  | Fun (Call (Literal (Atom f), args)) -> call program scope modl f args
  | _ -> fail "unsupported goal"

and builtin program modl scope name args =
  match (name, args) with
  | "conj", [ goals ] ->
      combine program modl scope "$mk_conj" "$g_true" (list_elements goals)
  | "disj", [ goals ] ->
      combine program modl scope "$mk_disj" "$g_false" (list_elements goals)
  | ( "call-with-fresh",
      [ Fun (Lambda [ { patterns = [ Variable var ]; body = [ body ]; _ } ]) ] )
    ->
      let body = goal program modl (scope @ [ var ]) body in
      let name =
        Printf.sprintf "$lambda%d" (List.length program.lambda_names)
      in
      program.lambda_names <- name :: program.lambda_names;
      Buffer.add_string program.lambdas (predicate_header name body);
      Printf.sprintf "(call $mk_fresh (ref.func %s) (local.get $env))" name
  | "query-variable", [ var; Literal (Atom name); body ] ->
      Printf.sprintf "(call $mk_query (i32.const %d) %s %s)" (atom program name)
        (term program scope var)
        (goal program modl scope body)
  | _ -> Printf.sprintf "(call $karuta.%s %s)" name (env program scope args)

(* Mirrors conj/1 and disj/1 in karuta.lfe: fold right, delaying each goal *)
and combine program modl scope constructor unit goals =
  List.fold_right
    (fun g acc ->
      Printf.sprintf "(call %s (call $mk_delay %s) %s)" constructor
        (goal program modl scope g)
        acc)
    goals
    (Printf.sprintf "(global.get %s)" unit)

and call program scope modl name args =
  match Hashtbl.find_opt program.functions (modl, name, List.length args) with
  | Some f -> Printf.sprintf "(call %s %s)" f (env program scope args)
  | None when modl = "sakura" -> fail "Sakura is not supported"
  | None ->
      fail
        (Printf.sprintf "unknown predicate %s:%s/%d" modl name
           (List.length args))

let module_name forms =
  match
    List.find_map (function Form.ModuleAttr m -> Some m | _ -> None) forms
  with
  | Some m -> m
  | None -> fail "module without a name"

let functions forms =
  List.filter_map
    (function
      | Form.Function { name; arity; clauses } -> Some (name, arity, clauses)
      | _ -> None)
    forms

(* Atom names, sorted so that ids compare like Erlang atoms, laid out as a
   table of (offset, length) pairs followed by the bytes themselves. *)
let atom_table atoms =
  let names = BatSet.String.elements atoms in
  let table_size = 8 * List.length names in
  let header = Buffer.create table_size and bytes = Buffer.create 256 in
  List.iter
    (fun name ->
      Buffer.add_int32_le header
        (Int32.of_int (table_size + Buffer.length bytes));
      Buffer.add_int32_le header (Int32.of_int (String.length name));
      Buffer.add_string bytes name)
    names;
  Buffer.add_buffer header bytes;
  let data = Buffer.contents header in
  let escaped = Buffer.create (3 * String.length data) in
  String.iter
    (fun c -> Buffer.add_string escaped (Printf.sprintf "\\%02x" (Char.code c)))
    data;
  ( BatMap.String.of_list (List.mapi (fun i name -> (name, i)) names),
    String.length data,
    Buffer.contents escaped )

let program ~root_module ~(query : Ast.head) (modules : Form.t list list) :
    string =
  let atoms =
    List.fold_left
      (fun acc forms ->
        List.fold_left
          (fun acc (_, _, clauses) ->
            List.fold_left
              (fun acc { Clause.body; _ } ->
                List.fold_left atoms_of_expr acc body)
              acc clauses)
          acc (functions forms))
      BatSet.String.empty modules
  in
  let atoms, data_size, data = atom_table atoms in
  let program =
    {
      atoms;
      functions = Hashtbl.create 64;
      lambdas = Buffer.create 4096;
      lambda_names = [];
    }
  in
  List.iter
    (fun forms ->
      let modl = module_name forms in
      List.iter
        (fun (name, arity, _) ->
          Hashtbl.replace program.functions (modl, name, arity)
            (Printf.sprintf "$f%d" (Hashtbl.length program.functions)))
        (functions forms))
    modules;
  let predicates = Buffer.create 4096 in
  List.iter
    (fun forms ->
      let modl = module_name forms in
      List.iter
        (function
          | name, arity, [ { Clause.patterns; body = [ body ]; _ } ] ->
              let scope =
                List.map
                  (function Pattern.Variable v -> v | _ -> "_")
                  patterns
              in
              Buffer.add_string predicates
                (predicate_header
                   (Hashtbl.find program.functions (modl, name, arity))
                   (goal program modl scope body))
          | name, _, _ -> fail ("unsupported function shape for " ^ name))
        (functions forms))
    modules;
  let entry =
    match
      Hashtbl.find_opt program.functions
        (root_module, query.name, query.arity + 1)
    with
    | Some f -> f
    | None -> fail ("could not find the query of module " ^ root_module)
  in
  let out_base = (data_size + 15) land lnot 15 in
  String.concat "\n"
    [
      "(module";
      Runtime.source;
      Buffer.contents predicates;
      Buffer.contents program.lambdas;
      (if program.lambda_names = [] then ""
       else "(elem declare func " ^ String.concat " " program.lambda_names ^ ")");
      Printf.sprintf "(memory (export \"memory\") %d)" ((out_base / 65536) + 1);
      Printf.sprintf
        "(global $out_base (export \"out_base\") i32 (i32.const %d))" out_base;
      Printf.sprintf "(data (i32.const 0) \"%s\")" data;
      Printf.sprintf
        "(func (export \"run\") (param $limit i32) (result i32)\n\
        \  (call $run (call %s %s) (local.get $limit)))"
        entry
        (env program
           (List.init (query.arity + 1) (Fun.const "_"))
           (List.init (query.arity + 1) (Fun.const (Literal Nil : Expr.t))));
      ")";
    ]
