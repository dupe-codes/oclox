let print_literal literal =
  match literal with
  | Some (Value.String s) -> s
  | Some (Value.Float f) -> string_of_float f
  | Some (Value.Bool bool) -> string_of_bool bool
  | Some (Value.Function { name; _ }) -> Printf.sprintf "<fn %s>" name
  | Some (Value.Native ({ name; _ }, _)) -> Printf.sprintf "<native fn %s>" name
  | None -> "nil"

let rec parenthesize lexeme exprs =
  let expr_strs = List.map print_expr exprs in
  Printf.sprintf "(%s %s)" lexeme (String.concat " " expr_strs)

and print_expr ast =
  match ast with
  | Expression.Binary (l, op, r) ->
      parenthesize ("Binary: " ^ Token.get_lexeme op) [ l; r ]
  | Grouping expr -> parenthesize "group" [ expr ]
  | Literal literal -> print_literal literal
  | Unary (op, r) -> parenthesize (Token.get_lexeme op) [ r ]
  | Variable name -> Token.to_string name
  | Assign (token, expr) ->
      parenthesize ("Assign " ^ Token.to_string token) [ expr ]
  | Logical (l, op, r) ->
      parenthesize ("Logical: " ^ Token.get_lexeme op) [ l; r ]
  | Call (callee, paren, arguments) ->
      parenthesize ("Call: " ^ Token.get_lexeme paren) (callee :: arguments)

let rec print_statement = function
  | Statement.Expression expr -> print_expr expr
  | Print expr -> Printf.sprintf "(print %s)" (print_expr expr)
  | If (condition, then_branch, else_branch) ->
      Printf.sprintf "(if %s %s %s)" (print_expr condition)
        (print_statement then_branch)
        (Option.fold ~none:"nil" ~some:print_statement else_branch)
  | While (condition, body) ->
      Printf.sprintf "(while %s %s)" (print_expr condition)
        (print_statement body)
  | Function { name; params; body } ->
      Printf.sprintf "(fn %s %s %s)" (Token.to_string name)
        (String.concat " " (List.map Token.to_string params))
        (print_statements body)
  | Return (_, expr) ->
      Printf.sprintf "(return %s)"
        (Option.fold ~none:"nil" ~some:print_expr expr)
  | Var (token, expr) ->
      Printf.sprintf "(var %s %s)" (Token.to_string token)
        (Option.fold ~none:"nil" ~some:print_expr expr)
  | Block stmts -> Printf.sprintf "(block %s)" (print_statements stmts)

and print_statements statements =
  let statement_strs = List.map print_statement statements in
  String.concat "\n" statement_strs
