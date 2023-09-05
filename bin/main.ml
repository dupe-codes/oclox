(*NOTE:
  - Error handling with immutable state will be tough. Punt on it
    until error cases arise, and think about how to deal with it
    then. Maybe have "error" fields in the scanner and parser
    types that are set to None when no error has occurred, and
    Some error when an error has occurred.
*)

let run source =
  let open Oclox in
  let scanner = Scanner.init source in
  let scan_result = Scanner.scan_tokens scanner in
  match scan_result with
  | Ok tokens ->
      let _ =
        List.iter
          (fun token -> Printf.printf "%s\n" (Token.to_string token))
          tokens
      in
      true
  | Error _ -> false

let rec run_prompt () =
  let _ = Printf.printf "> %!" in
  let line = Stdio.In_channel.input_line Stdio.stdin in
  match line with
  | None -> ()
  | Some line ->
      let _ = run line in
      run_prompt ()

let run_file file_name =
  let file_contents = Stdio.In_channel.read_all file_name in
  if not (run file_contents) then exit 65

let _ =
  let args_length = Array.length Sys.argv in
  let errored_invocation = if args_length > 2 then true else false in
  if errored_invocation then
    let _ = Printf.printf "Usage: oclox [script]" in
    exit 64
  else if Array.length Sys.argv = 2 then run_file Sys.argv.(1)
  else run_prompt ()
