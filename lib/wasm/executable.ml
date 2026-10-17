open Shared.Compiler

type payload = {
  persist : Persist.raw;
  modules : forms list;
  query : Ast.head;
  filename : string;
  root_module : string;
}

(* Node runner for the module emitted next to it; in a browser, instantiate
   the .wasm the same way and read `length` bytes of memory from out_base. *)
let runner =
  {|#!/usr/bin/env node
const bytes = require("fs").readFileSync(__filename + ".wasm");
WebAssembly.instantiate(bytes).then(({ instance: { exports } }) => {
  const limit = process.argv[2];
  const length = exports.run(
    limit === undefined || limit === "inf" ? -1 : parseInt(limit, 10),
  );
  process.stdout.write(
    Buffer.from(exports.memory.buffer, exports.out_base.value, length),
  );
});
|}

let assemble wat wasm =
  let args =
    [|
      "wasm-as";
      "--enable-gc";
      "--enable-reference-types";
      "--enable-multivalue";
      wat;
      "-o";
      wasm;
    |]
  in
  match
    Unix.create_process "wasm-as" args Unix.stdin Unix.stdout Unix.stderr
  with
  | pid -> Posix.treat_pid pid
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      Logger.simply_error
        "Could not find 'wasm-as' (from binaryen) to assemble the module";
      exit 1

(* Assembles in a scratch directory, then persists the binary and its runner
   side by side, wherever the caller persists executables. *)
let emit { persist; modules; query; filename; root_module } : unit Error.attempt
    =
  let wat = Filename.temp_file "karuta" ".wat" in
  let wasm = Filename.temp_file "karuta" ".wasm" in
  Out_channel.with_open_text wat (fun c ->
      Out_channel.output_string c
      @@ Codegen.program ~root_module ~query
      @@ List.map BatFingerTree.to_list modules);
  let result = assemble wat wasm in
  let binary = In_channel.with_open_bin wasm In_channel.input_all in
  Sys.remove wat;
  Sys.remove wasm;
  match result with
  | Ok () ->
      persist (filename ^ ".wasm") binary;
      Ok (persist filename runner)
  | Error status -> Error (Error.FailedToRunExternalProgram ("wasm-as", status))
