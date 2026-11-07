open Alcotest

module Logger = Lib.Logger.Make (struct
  include Lib.Logger.Log

  let format_message prefix loc msg =
    prefix ^ (if Option.is_some loc then "located: " else "") ^ msg

  let format_error_message = format_message
  let format_warning_message = format_message
  let format_info_message = format_message
  let format_unreachable_message = format_message
  let format_debug_message = format_message
end)

let loc = Lib.Location.dummy

let with_output f =
  let filename, output = Filename.open_temp_file "karuta-logger-" ".log" in
  let previous_messages =
    Atomic.exchange Lib.Logger.Log.messages_by_file BatMap.String.empty
  in
  Stdlib.flush stdout;
  let saved_stdout = Unix.dup Unix.stdout in
  Fun.protect ~finally:(fun () ->
      Stdlib.flush stdout;
      Unix.dup2 saved_stdout Unix.stdout;
      Unix.close saved_stdout;
      close_out output;
      Sys.remove filename;
      Atomic.set Lib.Logger.Log.messages_by_file previous_messages)
  @@ fun () ->
  Unix.dup2 (Unix.descr_of_out_channel output) Unix.stdout;
  let read_output () =
    Stdlib.flush stdout;
    In_channel.with_open_text filename In_channel.input_all
  in
  f read_output

let preserves_call_order () =
  with_output @@ fun read_output ->
  Logger.with_min_level Lib.Logger.Level.Debug (fun () ->
      Logger.in_file "example.krt" (fun () ->
          Logger.debug loc "first";
          Logger.simply_debug "second";
          Logger.info loc "third";
          Logger.simply_info "fourth"));
  check string "all messages wait for flush" "" (read_output ());
  Logger.flush ();
  check string "mixed messages retain call order"
    "[DEBUG] located: first\n\
     [DEBUG] second\n\
     [INFO] located: third\n\
     [INFO] fourth\n"
    (read_output ())

let prints_without_file_context () =
  with_output @@ fun read_output ->
  Logger.simply_error "first";
  check string "simple message prints immediately" "[ERROR] first\n"
    (read_output ());
  Logger.error loc "second";
  let expected = "[ERROR] first\n[ERROR] located: second\n" in
  check string "located message prints immediately" expected (read_output ());
  Logger.flush ();
  check string "immediate messages are not buffered" expected (read_output ())

let filters_messages () =
  with_output @@ fun read_output ->
  Logger.with_min_level Lib.Logger.Level.Info (fun () ->
      Logger.simply_debug "hidden outside file";
      Logger.debug loc "hidden located outside file";
      Logger.in_file "example.krt" (fun () ->
          Logger.debug loc "hidden located inside file";
          Logger.simply_debug "hidden inside file";
          Logger.simply_info "visible"));
  check string "filtered messages produce no immediate output" ""
    (read_output ());
  Logger.flush ();
  check string "only enabled messages are buffered" "[INFO] visible\n"
    (read_output ())

let preserves_file_grouping () =
  with_output @@ fun read_output ->
  Logger.in_file "b.krt" (fun () ->
      Logger.info loc "b first";
      Logger.in_file "a.krt" (fun () -> Logger.simply_info "a first");
      Logger.simply_info "b second");
  check string "all files wait for flush" "" (read_output ());
  Logger.flush ();
  check string "files retain their flush order and context"
    "[INFO] a first\n[INFO] located: b first\n[INFO] b second\n"
    (read_output ())

let tests =
  [
    test_case "preserves call order" `Quick preserves_call_order;
    test_case "prints without file context" `Quick prints_without_file_context;
    test_case "filters messages" `Quick filters_messages;
    test_case "preserves file grouping" `Quick preserves_file_grouping;
  ]
