(* Exposes `karuta.compile(source)` to JavaScript: compiles a single Karuta
   module ending in a query to WebAssembly text, which the page assembles with
   binaryen.js and runs. *)

open Lib
open Js_of_ocaml

(* jsoo keeps an in-memory filesystem under /static/, so the Logger can quote
   the source in its error messages *)
let filepath = "/static/repl.krt"

(* Same pipeline as Executor.compile for a single in-memory module *)
let compile source =
  if Sys.file_exists filepath then
    Sys_js.update_file ~name:filepath ~content:source
  else Sys_js.create_file ~name:filepath ~content:source;
  Eio_mock.Backend.run @@ fun () ->
  let module K = Shared.Compiler.Make (Karuta.Module) in
  let modules = ref [] in
  let persist : Shared.Compiler.Persist.both =
    {
      beam = (fun _ forms -> modules := BatFingerTree.to_list forms :: !modules);
      executable = (fun _ _ -> ());
    }
  in
  let ({ module_ } : _ Shared.Preprocessor.output) =
    Parser.parse filepath source
    |> K.preprocess_clauses (Shared.Preprocessor.initialize filepath)
  in
  let root_module = ModuleName.of_filepath filepath in
  let { Location.content; loc } =
    BatMap.String.find root_module
    @@ K.compile_files persist (BatMap.String.singleton filepath module_)
  in
  match
    Karuta.Lookup.check_dependency_cycle loc.filename @@ fun () ->
    Lazy.force content
  with
  | { query = None; _ } -> Error "There is no query to run."
  | { query = Some query; _ } ->
      Ok (Wasm.Codegen.program ~root_module ~query:query.content !modules)

let () =
  let log = Buffer.create 1024 in
  Sys_js.set_channel_flusher stdout (Buffer.add_string log);
  let result ok text =
    Js.Unsafe.obj
      [|
        ("ok", Js.Unsafe.inject (Js.bool ok));
        ("text", Js.Unsafe.inject (Js.string text));
      |]
  in
  Js.export "karuta"
    (Js.Unsafe.obj
       [|
         ( "compile",
           Js.Unsafe.inject
           @@ Js.wrap_callback (fun source ->
               Buffer.clear log;
               let outcome =
                 try compile (Js.to_string source)
                 with e ->
                   Error ("Compilation failed: " ^ Printexc.to_string e)
               in
               flush stdout;
               match outcome with
               | Ok wat -> result true wat
               | Error message ->
                   result false
                     (if Buffer.length log = 0 then message
                      else Buffer.contents log)) );
       |])
