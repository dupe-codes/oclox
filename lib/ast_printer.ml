let print_literal literal =
  match literal with
  | Some (Value.String s) -> s
  | Some (Value.Float f) -> string_of_float f
  | Some (Value.Bool bool) -> string_of_bool bool
  | None -> "nil"

let rec parenthesize lexeme exprs =
  let expr_strs = List.map print_expr exprs in
  Printf.sprintf "(%s %s)" lexeme (String.concat " " expr_strs)

and print_expr ast =
  match ast with
  | Expression.Binary (l, op, r) -> parenthesize (Token.get_lexeme op) [ l; r ]
  | Grouping expr -> parenthesize "group" [ expr ]
  | Literal literal -> print_literal literal
  | Unary (op, r) -> parenthesize (Token.get_lexeme op) [ r ]
  | Variable name -> Token.to_string name
  | Assign (token, expr) ->
      parenthesize ("Assign " ^ Token.to_string token) [ expr ]

let print_statement = function
  | Statement.Expression expr -> print_expr expr
  | Print expr -> Printf.sprintf "(print %s)" (print_expr expr)
  | Var (token, expr) ->
      Printf.sprintf "(var %s %s)" (Token.to_string token)
        (Option.fold ~none:"nil" ~some:print_expr expr)

let print_statements statements =
  let statement_strs = List.map print_statement statements in
  String.concat "\n" statement_strs
