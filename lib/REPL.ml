(*
Modification from https://github.com/ocaml-community/lambda-term/blob/master/examples/repl.ml
Replicating the copyright notice
*)

(*
Copyright (c) 2011, Jeremie Dimino <jeremie@dimino.org>
All rights reserved.
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Jeremie Dimino nor the names of his
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE AUTHOR AND CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

open React
open Lwt
open LTerm_text

(* A simple model of an interpreter. It maintains some state, and exposes a function
 *   eval : state -> input -> (new_state, output) *)
module Interpreter = struct
  type state = (Compiler.t * Machine.t) option

  let initial_state = None

  (* Check if command is complete (ends with question mark, or is a meta-command) *)
  let is_complete cmd =
    let trimmed = String.trim cmd in
    String.length trimmed > 0
    && (trimmed.[0] = '\\' || String.ends_with ~suffix:"?" trimmed)

  let eval state s =
    let trimmed = String.trim s in
    if String.length trimmed = 0 then (state, "")
    else if trimmed.[0] = '\\' then
      (* Meta-command *)
      if trimmed = "\\q" then raise Exit
      else (state, "Executed meta-command: " ^ trimmed)
    else
      match Executor.continue trimmed state with
      | Some (_, computer) as new_state ->
          (new_state, Crawler.Tabulation.query_string computer)
      | None -> (state, "")
end

(* Create a prompt based on the current interpreter state *)
let make_prompt _state _buffer_empty =
  let prompt = "karuta> " in
  eval [ B_bold true; S prompt; E_bold ]

(* Format the interpreter output for REPL display *)
let make_output out = if String.length out > 0 then eval [ S out ] else eval []

class read_line ~term ~history ~state ~buffer_empty =
  object (self)
    inherit LTerm_read_line.read_line ~history ()
    inherit [Zed_string.t] LTerm_read_line.term term
    method! show_box = false
    initializer self#set_prompt (S.const (make_prompt state buffer_empty))
  end

let rec loop term history state buffer =
  let buffer_empty = String.length buffer = 0 in

  Lwt.catch
    (fun () ->
      let rl =
        new read_line
          ~term
          ~history:(LTerm_history.contents history)
          ~state ~buffer_empty
      in
      rl#run >|= fun command -> Some command)
    (function Sys.Break -> return None | exn -> Lwt.fail exn)
  >>= function
  | Some command ->
      let line = Zed_string.to_utf8 command in
      let new_buffer =
        if String.length buffer = 0 then line else buffer ^ "\n" ^ line
      in

      if Interpreter.is_complete new_buffer then
        (* Execute complete command *)
        Lwt.catch
          (fun () ->
            let new_state, out = Interpreter.eval state new_buffer in
            LTerm.fprintls term (make_output out) >>= fun () ->
            if String.length new_buffer > 0 && new_buffer.[0] <> '\\' then
              LTerm_history.add history command;
            loop term history new_state "")
          (function Exit -> Lwt.return () | exn -> Lwt.fail exn)
      else
        (* Continue building multi-line command *)
        loop term history state new_buffer
  | None -> loop term history state buffer

let program_loader term files () =
  loop term (LTerm_history.create []) (Executor.load_many_decls files) ""

let main (files : string list) =
  LTerm_inputrc.load () >>= fun () ->
  Lwt.catch
    (fun () ->
      Lazy.force LTerm.stdout >>= fun term ->
      LTerm.fprintls term (eval [ S "Karuta REPL\n" ])
      >>= program_loader term files)
    (function
      | LTerm_read_line.Interrupt -> Lwt.return () | exn -> Lwt.fail exn)
