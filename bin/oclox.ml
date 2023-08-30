(*NOTE:
  - Error handling with immutable state will be tough. Punt on it
    until error cases arise, and think about how to deal with it
    then. Maybe have "error" fields in the scanner and parser
    types that are set to None when no error has occurred, and
    Some error when an error has occurred.
*)

let _ =
  let out = Parser.hello () in
  print_endline out
