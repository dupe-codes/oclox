type t = { tokens : Token.t list }

exception ParseError

let init tokens = { tokens }
let peek parser = List.hd parser.tokens

let is_at_end parser =
  let peek_token = peek parser in
  peek_token.token_type = Token.EOF

let advance parser =
  let next_token = peek parser in
  ( next_token,
    if not (is_at_end parser) then { tokens = List.tl parser.tokens }
    else parser )

let synchronize parser =
  (* advance passed token that caused the parsing error *)
  let token, parser = advance parser in
  let rec loop (previous : Token.t) parser =
    if is_at_end parser || previous.token_type = Token.SEMICOLON then parser
    else
      let next_token = peek parser in
      match next_token.token_type with
      | Token.CLASS | Token.FUN | Token.VAR | Token.FOR | Token.IF | Token.WHILE
      | Token.PRINT | Token.RETURN ->
          parser
      | _ ->
          let token, parser = advance parser in
          loop token parser
  in
  loop token parser

let match_token parser targets =
  if is_at_end parser then (None, parser)
  else
    let head = List.hd parser.tokens in
    if List.mem head.token_type targets then
      let remaining = List.tl parser.tokens in
      (Some head, { tokens = remaining })
    else (None, parser)

let rec match_loop ?(expr_fn = fun l op r -> Expression.Binary (l, op, r))
    parser expr targets right_fn =
  let maybe_token, parser = match_token parser targets in
  match maybe_token with
  | Some operator ->
      let right, parser = right_fn parser in
      let expr = expr_fn expr operator right in
      match_loop parser expr targets right_fn
  | None -> (expr, parser)

let parse_error (token : Token.t) msg =
  let _ = Lox_error.init_for_token token msg in
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

let rec grouping parser =
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
      grouping parser
  | Token.IDENTIFIER n ->
      let _, parser = consume (Token.IDENTIFIER n) parser in
      (Expression.Variable next_token, parser)
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

and and_expr parser =
  let expr, parser = equality parser in
  match_loop parser expr [ Token.AND ] equality ~expr_fn:(fun l op r ->
      Expression.Logical (l, op, r))

and or_expr parser =
  let expr, parser = and_expr parser in
  match_loop parser expr [ Token.OR ] and_expr ~expr_fn:(fun l op r ->
      Expression.Logical (l, op, r))

and assignment parser =
  let expr, parser = or_expr parser in
  let maybe_token, parser = match_token parser [ Token.EQUAL ] in
  match maybe_token with
  | None -> (expr, parser)
  | Some equals -> (
      let value, parser = assignment parser in
      match expr with
      | Expression.Variable name -> (Expression.Assign (name, value), parser)
      | _ -> raise (parse_error equals "Invalid assignment target"))

and expression parser = assignment parser

let print_statement parser =
  let expr, parser = expression parser in
  let _, parser =
    consume_with_msg Token.SEMICOLON parser "Expect ';' after value."
  in
  (Statement.Print expr, parser)

let expression_statement parser =
  let expr, parser = expression parser in
  let _, parser =
    consume_with_msg Token.SEMICOLON parser "Expect ';' after expression."
  in
  (Statement.Expression expr, parser)

let consume_identifier parser =
  let next_token = peek parser in
  match next_token.token_type with
  | Token.IDENTIFIER _ ->
      (List.hd parser.tokens, { tokens = List.tl parser.tokens })
  | _ -> raise (parse_error next_token "Expect variable name")

let var_declaration parser =
  let name_token, parser = consume_identifier parser in
  let maybe_initializer, parser = match_token parser [ Token.EQUAL ] in
  let initializer_expr, parser =
    match maybe_initializer with
    | Some _ ->
        let expr, parser = expression parser in
        (Some expr, parser)
    | None -> (None, parser)
    (* TODO: Maybe use null literal as initializer expr here instead of option type*)
  in
  let _, parser =
    consume_with_msg Token.SEMICOLON parser
      "Expect ';' after variable declaration."
  in
  (Statement.Var (name_token, initializer_expr), parser)

let rec declaration parser =
  try
    let maybe_token, parser = match_token parser [ Token.VAR ] in
    match maybe_token with
    | Some _ ->
        let stmt, parser = var_declaration parser in
        (Some stmt, parser)
    | None -> statement parser
  with ParseError ->
    let parser = synchronize parser in
    (None, parser)

and block parser =
  let rec loop stmts parser =
    if is_at_end parser then (List.rev stmts, parser)
    else if (peek parser).token_type = Token.RIGHT_BRACE then
      consume Token.RIGHT_BRACE parser |> fun (_, parser) ->
      (List.rev stmts, parser)
    else
      let stmt, parser = declaration parser in
      loop (stmt :: stmts) parser
  in
  loop [] parser

and if_statement parser =
  let _, parser = consume Token.LEFT_PAREN parser in
  let condition, parser = expression parser in
  let _, parser = consume Token.RIGHT_PAREN parser in
  let then_branch, parser = statement parser in
  if Option.is_none then_branch then (None, parser)
  else
    let else_branch, parser =
      match_token parser [ Token.ELSE ] |> fun (maybe_token, parser) ->
      match maybe_token with
      | Some _ -> statement parser
      | None -> (None, parser)
    in
    ( Some (Statement.If (condition, Option.get then_branch, else_branch)),
      parser )

and while_statement parser =
  let _, parser = consume Token.LEFT_PAREN parser in
  let condition, parser = expression parser in
  let _, parser = consume Token.RIGHT_PAREN parser in
  let body, parser = statement parser in
  if Option.is_none body then (None, parser)
  else (Some (Statement.While (condition, Option.get body)), parser)

and statement parser =
  let maybe_token, parser =
    match_token parser [ Token.PRINT; Token.LEFT_BRACE; Token.IF; Token.WHILE ]
  in
  match maybe_token with
  | Some { token_type = Token.PRINT; _ } ->
      let stmt, parser = print_statement parser in
      (Some stmt, parser)
  | Some { token_type = Token.IF; _ } ->
      let stmt, parser = if_statement parser in
      (stmt, parser)
  | Some { token_type = Token.WHILE; _ } ->
      let stmt, parser = while_statement parser in
      (stmt, parser)
  | Some { token_type = Token.LEFT_BRACE; _ } ->
      let statements, parser = block parser in
      if List.exists Option.is_none statements then (None, parser)
      else
        ( Some
            (Statement.Block
               (List.filter Option.is_some statements |> List.map Option.get)),
          parser )
  | Some _ | None ->
      let stmt, parser = expression_statement parser in
      (Some stmt, parser)

let parse parser =
  let rec loop parser statements errored =
    if is_at_end parser then
      if not errored then Some (List.rev statements) else None
    else
      let statement, parser = declaration parser in
      match statement with
      | Some stmt -> loop parser (stmt :: statements) (errored || false)
      | None -> loop parser statements true
  in
  loop parser [] false
