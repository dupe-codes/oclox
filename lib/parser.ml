type t = { tokens : Token.t list }

exception ParseError

let init tokens = { tokens }
let peek parser = List.hd parser.tokens

let is_at_end parser =
  let peek_token = peek parser in
  peek_token.token_type = Token.EOF

let match_token parser targets =
  if is_at_end parser then (None, parser)
  else
    let head = List.hd parser.tokens in
    if List.mem head.token_type targets then
      let remaining = List.tl parser.tokens in
      (Some head, { tokens = remaining })
    else (None, parser)

let rec match_loop parser expr targets right_fn =
  let maybe_token, parser = match_token parser targets in
  match maybe_token with
  | Some operator ->
      let right, parser = right_fn parser in
      let expr = Expression.Binary (expr, operator, right) in
      match_loop parser expr targets right_fn
  | None -> (expr, parser)

let parse_error (token : Token.t) msg =
  let _ = Error.init_for_token token msg in
  raise ParseError

let consume_with_msg expected_token parser err_msg =
  let next_token = peek parser in
  if next_token.token_type = expected_token then
    (List.hd parser.tokens, { tokens = List.tl parser.tokens })
  else raise (parse_error next_token err_msg)

let consume expected_type parser =
  consume_with_msg expected_type parser
    ("Expected " ^ Token.type_to_string expected_type)

let create_literal constructor value =
  Expression.Literal (Some (constructor value))

let rec parse_grouping parser =
  let expr, parser = expression parser in
  let _, parser =
    consume_with_msg Token.RIGHT_PAREN parser "Expect ')' after expression."
  in
  (Expression.Grouping expr, parser)

and primary parser =
  let next_token = peek parser in
  match next_token.token_type with
  | Token.FALSE ->
      let _, parser = consume Token.FALSE parser in
      (create_literal (fun x -> Bool x) false, parser)
  | Token.TRUE ->
      let _, parser = consume Token.TRUE parser in
      (create_literal (fun x -> Bool x) true, parser)
  | Token.NIL ->
      let _, parser = consume Token.NIL parser in
      (Expression.Literal None, parser)
  | Token.NUMBER n ->
      let _, parser = consume (Token.NUMBER n) parser in
      (create_literal (fun n -> Float n) n, parser)
  | Token.STRING s ->
      let _, parser = consume (Token.STRING s) parser in
      (create_literal (fun s -> String s) s, parser)
  | Token.LEFT_PAREN ->
      let _, parser = consume Token.LEFT_PAREN parser in
      parse_grouping parser
  | _ -> raise (parse_error next_token "Expected expression")

and unary parser =
  let maybe_token, parser = match_token parser [ Token.BANG; Token.MINUS ] in
  match maybe_token with
  | Some operator ->
      let right, parser = unary parser in
      (Expression.Unary (operator, right), parser)
  | None ->
      let expr, parser = primary parser in
      (expr, parser)

and factor parser =
  let expr, parser = unary parser in
  match_loop parser expr [ Token.SLASH; Token.STAR ] unary

and term parser =
  let expr, parser = factor parser in
  match_loop parser expr [ Token.MINUS; Token.PLUS ] factor

and comparison parser =
  let expr, parser = term parser in
  match_loop parser expr
    [ Token.GREATER; Token.GREATER_EQUAL; Token.LESS; Token.LESS_EQUAL ]
    term

and equality parser =
  let expr, parser = comparison parser in
  match_loop parser expr [ Token.BANG_EQUAL; Token.EQUAL_EQUAL ] comparison

and expression parser = equality parser

let parse parser =
  let expr, _ = expression parser in
  Some expr
