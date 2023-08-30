type t = { source : string; start : int; current : int; line : int }

let init source = { source; start = 0; current = 0; line = 1 }
let is_at_end scanner = scanner.current >= String.length scanner.source

let advance scanner =
  let ch = scanner.source.[scanner.current] in
  (ch, { scanner with current = scanner.current + 1 })

let make_token scanner token_type =
  let token : Token.t = { token_type; line = scanner.line } in
  Ok token

let scan_token scanner =
  let ch, scanner = advance scanner in
  (*let lexeme =*)
  (*String.sub scanner.source scanner.start (scanner.current - scanner.start)*)
  (*in*)
  let result =
    match ch with
    | '(' -> make_token scanner Token.LEFT_PAREN
    | ')' -> make_token scanner Token.RIGHT_PAREN
    | '{' -> make_token scanner Token.LEFT_BRACE
    | '}' -> make_token scanner Token.RIGHT_BRACE
    | ',' -> make_token scanner Token.COMMA
    | '.' -> make_token scanner Token.DOT
    | '-' -> make_token scanner Token.MINUS
    | '+' -> make_token scanner Token.PLUS
    | ';' -> make_token scanner Token.SEMICOLON
    | '*' -> make_token scanner Token.STAR
    | _ -> Error.init scanner.line "Unexpected character."
  in
  Result.map
    (fun token -> ({ scanner with start = scanner.current }, token))
    result

let rec scan_tokens_rec scanner tokens =
  if is_at_end scanner then
    let token = make_token scanner Token.EOF in
    Result.map (fun token -> token :: tokens) token
  else
    let token_result = scan_token scanner in
    Result.bind token_result (fun (scanner, token) ->
        scan_tokens_rec scanner (token :: tokens))

let scan_tokens scanner =
  Result.map (fun tokens -> List.rev tokens) (scan_tokens_rec scanner [])
