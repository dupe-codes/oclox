let print_literal literal =
  match literal with
  | Some (Value.String s) -> s
  | Some (Value.Float f) -> string_of_float f
  | Some (Value.Bool bool) -> string_of_bool bool
  | None -> "nil"

let rec parenthesize lexeme exprs =
  let expr_strs = List.map print exprs in
  Printf.sprintf "(%s %s)" lexeme (String.concat " " expr_strs)

and print ast =
  match ast with
  | Expression.Binary (l, op, r) -> parenthesize (Token.get_lexeme op) [ l; r ]
  | Expression.Grouping expr -> parenthesize "group" [ expr ]
  | Expression.Literal literal -> print_literal literal
  | Expression.Unary (op, r) -> parenthesize (Token.get_lexeme op) [ r ]
