open Lib.Interactive

let styled = Unix.isatty Unix.stdout

let table (solutions : Runtime.solution list) : PrintBox.t =
  let open PrintBox in
  let cell = pad' ~col:1 ~lines:0 in
  let header = List.map (fun (name, _) -> line_with_style Style.bold name) in
  let row = List.map (fun (_, value) -> line value) in
  grid_l ~pad:cell ~bars:true
    (header (List.hd solutions) :: List.map row solutions)
  |> frame

let print : Session.outcome -> unit = function
  | Defined | Rejected -> ()
  | Failed e -> print_endline @@ "Runtime error: " ^ e
  | Answers { solutions = []; _ } -> print_endline "false."
  | Answers { solutions = [] :: _; _ } -> print_endline "true."
  | Answers { solutions; exhausted } ->
      PrintBox_text.output ~style:styled stdout (table solutions);
      print_newline ();
      if not exhausted then print_endline "; for more"

let rec read buffer =
  let complete s =
    let s = String.trim s in
    s = ";" || String.ends_with ~suffix:"." s || String.ends_with ~suffix:"?" s
  in
  print_string (if buffer = "" then "karuta> " else "      | ");
  flush stdout;
  match In_channel.input_line stdin with
  | None -> None
  | Some line ->
      let buffer = if buffer = "" then line else buffer ^ "\n" ^ line in
      if complete buffer then Some (String.trim buffer) else read buffer

let run ~(path : string) ~(page_size : int) =
  let session = Session.start ~path in
  Fun.protect ~finally:(fun () -> Session.stop session) @@ fun () ->
  let rec loop () =
    match read "" with
    | None -> print_newline ()
    | Some ";" ->
        print (Session.next session ~limit:page_size);
        loop ()
    | Some input ->
        print (Session.eval session ~limit:page_size input);
        loop ()
  in loop ()
