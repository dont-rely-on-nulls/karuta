type t = { pid : int; requests : out_channel; replies : in_channel }
type solution = (string * string) list
type page = { solutions : solution list; exhausted : bool }

let spawn ~(path : string) : t =
  let node_in, requests = Unix.pipe ~cloexec:true () in
  let replies, node_out = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process "erl"
      [| "erl"; "-noinput"; "-pa"; path; "-eval"; "interactive:start()." |]
      node_in node_out Unix.stderr
  in Unix.close node_in;
     Unix.close node_out;
     { pid
     ; requests = Unix.out_channel_of_descr requests
     ; replies = Unix.in_channel_of_descr replies }

let stop { pid; requests; replies } =
  close_out_noerr requests;
  close_in_noerr replies;
  let rec wait () =
    try ignore (Unix.waitpid [] pid)
    with Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
  in wait ()

let u32 n = String.init 4 (fun i -> Char.chr ((n lsr (8 * (3 - i))) land 0xff))

let request { requests; replies; _ } (frame : string) : string =
  output_string requests (u32 (String.length frame));
  output_string requests frame;
  flush requests;
  let size = really_input_string replies 4 in
  really_input_string replies (Int32.to_int (String.get_int32_be size 0))

let reply (frame : string) (k : string -> 'a) : ('a, string) result =
  let body = String.sub frame 1 (String.length frame - 1) in
  match frame.[0] with
  | 'e' -> Error body
  | tag when String.contains "os" tag -> Ok (k body)
  | tag -> Error (Printf.sprintf "Unknown reply tag '%c'" tag)

let load node (forms : string) : (unit, string) result =
  reply (request node ("l" ^ forms)) ignore

let query node ~(cursor : int) (module_ : string) : (unit, string) result =
  reply (request node ("q" ^ u32 cursor ^ module_)) ignore

(* FIXME: Improve this grueling function later. For loops and refs was
   the easy way out of this torture. *)
let decode_page (body : string) : page =
  let pos = ref 1 in
  let int bytes =
    let n = ref 0 in
    for i = 0 to bytes - 1 do
      n := (!n lsl 8) lor Char.code body.[!pos + i]
    done;
    pos := !pos + bytes;
    !n
  in
  let sized bytes =
    let size = int bytes in
    let s = String.sub body !pos size in
    pos := !pos + size;
    s
  in
  let rec bindings acc =
    function
    | 0 -> List.rev acc
    | n ->
        let name = sized 2 in
        let value = sized 4 in
        bindings ((name, value) :: acc) (n - 1)
  in
  let rec solutions acc =
    if !pos >= String.length body then List.rev acc
    else
      let count = int 2 in
      solutions (bindings [] count :: acc)
  in { exhausted = body.[0] = '\001'; solutions = solutions [] }

let next node ~(cursor : int) (limit : int) : (page, string) result =
  reply (request node ("n" ^ u32 cursor ^ u32 limit)) decode_page
