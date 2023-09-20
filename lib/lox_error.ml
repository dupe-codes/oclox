type error_type = Syntax_error | Runtime_error

exception Runtime of Token.t * string

let report line where msg =
  let _ =
    Printf.eprintf "[line %s] Error %s: %s\n%!" (string_of_int line) where msg
  in
  Error Syntax_error

let init line msg = report line "" msg

let init_for_token (token : Token.t) msg =
  match token.token_type with
  | Token.EOF -> report token.line "at end" msg
  | _ -> report token.line (" at '" ^ Token.get_lexeme token ^ "'") msg

let init_runtime (token : Token.t) msg =
  let _ = Printf.eprintf "%s\n[line %s]\n%!" msg (string_of_int token.line) in
  Error Runtime_error
