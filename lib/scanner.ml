(* TODO: Figure out where the pipeline operator |> can be used *)

type t = { source : string; start : int; current : int; line : int }

let init source = { source; start = 0; current = 0; line = 1 }
let is_at_end scanner = scanner.current >= String.length scanner.source

let advance scanner =
  let ch = scanner.source.[scanner.current] in
  (ch, { scanner with current = scanner.current + 1 })

let make_token scanner token_type =
  let token = Token.init token_type scanner.line in
  Ok ({ scanner with start = scanner.current }, token)

let match_lexeme scanner ch =
  if is_at_end scanner then (scanner, false)
  else if scanner.source.[scanner.current] <> ch then (scanner, false)
  else
    let scanner = { scanner with current = scanner.current + 1 } in
    (scanner, true)

let make_prefixed_token scanner next_ch prefix_token full_token =
  let scanner, matched = match_lexeme scanner next_ch in
  make_token scanner (if matched then full_token else prefix_token)

let peek scanner =
  if is_at_end scanner then '\000' else scanner.source.[scanner.current]

let peek_next scanner =
  if scanner.current + 1 >= String.length scanner.source then '\000'
  else scanner.source.[scanner.current + 1]

let rec advance_comment scanner =
  if is_at_end scanner then make_token scanner Token.COMMENT
  else
    let ch = peek scanner in
    if ch = '\n' then make_token scanner Token.COMMENT
    else
      let _, scanner = advance scanner in
      advance_comment scanner

let rec make_string_token scanner =
  if is_at_end scanner then Error.init scanner.line "Unterminated string."
  else
    let next_ch = peek scanner in
    if next_ch = '"' then
      let _, scanner = advance scanner in
      let value =
        String.sub scanner.source (scanner.start + 1)
          (scanner.current - scanner.start - 2)
      in
      make_token scanner (Token.STRING value)
    else
      let scanner =
        if next_ch = '\n' then { scanner with line = scanner.line + 1 }
        else scanner
      in
      let _, scanner = advance scanner in
      make_string_token scanner

let is_digit = function '0' .. '9' -> true | _ -> false
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false
let is_alphanumeric ch = is_digit ch || is_alpha ch

let rec make_number_token scanner =
  let peek_ch = peek scanner in
  if is_digit peek_ch || (peek_ch = '.' && is_digit (peek_next scanner)) then
    advance scanner |> fun (_, scanner) -> make_number_token scanner
  else
    let value =
      String.sub scanner.source scanner.start (scanner.current - scanner.start)
    in
    make_token scanner (Token.NUMBER (float_of_string value))

let rec make_identifier_token scanner =
  if is_alphanumeric (peek scanner) then
    advance scanner |> fun (_, scanner) -> make_identifier_token scanner
  else
    let value =
      String.sub scanner.source scanner.start (scanner.current - scanner.start)
    in
    make_token scanner (Token.get_identifier_type value)

(* NOTE: Maybe return an option Token.t to handle skipped tokens, rather than
   returning Token.WHITESPACE and the like *)
let scan_token scanner =
  let ch, scanner = advance scanner in
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
  | '!' -> make_prefixed_token scanner '=' Token.BANG Token.BANG_EQUAL
  | '=' -> make_prefixed_token scanner '=' Token.EQUAL Token.EQUAL_EQUAL
  | '<' -> make_prefixed_token scanner '=' Token.LESS Token.LESS_EQUAL
  | '>' -> make_prefixed_token scanner '=' Token.GREATER Token.GREATER_EQUAL
  | '/' ->
      let scanner, matched = match_lexeme scanner '/' in
      if matched then advance_comment scanner
      else make_token scanner Token.SLASH
  | ' ' | '\r' | '\t' -> make_token scanner Token.WHITESPACE
  | '\n' ->
      let scanner = { scanner with line = scanner.line + 1 } in
      make_token scanner Token.NEWLINE
  | '"' -> make_string_token scanner
  | ch ->
      if is_digit ch then make_number_token scanner
      else if is_alpha ch then make_identifier_token scanner
      else Error.init scanner.line "Unexpected character."

let is_ignored_token token_type =
  match token_type with
  | Token.WHITESPACE | Token.NEWLINE | Token.COMMENT -> true
  | _ -> false

let rec scan_tokens_rec scanner tokens =
  if is_at_end scanner then
    let token = make_token scanner Token.EOF in
    Result.map (fun (_, token) -> token :: tokens) token
  else
    let token_result = scan_token scanner in
    Result.bind token_result (fun (scanner, token) ->
        scan_tokens_rec scanner
          (if not (is_ignored_token token.token_type) then token :: tokens
           else tokens))

let scan_tokens scanner =
  Result.map (fun tokens -> List.rev tokens) (scan_tokens_rec scanner [])
