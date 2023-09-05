type t = { tokens : Token.t list }

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

let primary parser =
  (Expression.Literal (Some (Float 1234.)), { tokens = List.tl parser.tokens })

let rec unary parser =
  let maybe_token, parser = match_token parser [ Token.BANG; Token.MINUS ] in
  match maybe_token with
  | Some operator ->
      let right, parser = unary parser in
      (Expression.Unary (operator, right), parser)
  | None ->
      let expr, parser = primary parser in
      (expr, parser)

let factor parser =
  let expr, parser = unary parser in
  match_loop parser expr [ Token.SLASH; Token.STAR ] unary

let term parser =
  let expr, parser = factor parser in
  match_loop parser expr [ Token.MINUS; Token.PLUS ] factor

let comparison parser =
  let expr, parser = term parser in
  match_loop parser expr
    [ Token.GREATER; Token.GREATER_EQUAL; Token.LESS; Token.LESS_EQUAL ]
    term

let equality parser =
  let expr, parser = comparison parser in
  match_loop parser expr [ Token.BANG_EQUAL; Token.EQUAL_EQUAL ] comparison

let expression parser = equality parser

let parse parser =
  let expr, _ = expression parser in
  Some expr
