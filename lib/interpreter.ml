let apply_bang value =
  match value with
  | Some (Value.Bool x) -> Some (Value.Bool (not x))
  | None -> Some (Value.Bool true)
  | _ -> Some (Value.Bool false)

let apply_unary (token : Token.t) value =
  match (token.token_type, value) with
  | Token.MINUS, Some (Value.Float x) -> Some (Value.Float (-1. *. x))
  | Token.MINUS, _ ->
      raise (Lox_error.Runtime (token, "Operand must be a number"))
  | Token.BANG, v -> apply_bang v
  | _ -> failwith "Unimplemented unary operator"

let apply_minus op l r =
  match (l, r) with
  | Some (Value.Float l), Some (Value.Float r) -> Some (Value.Float (l -. r))
  | _ -> raise (Lox_error.Runtime (op, "Operands must be numbers"))

let apply_slash op l r =
  match (l, r) with
  | Some (Value.Float l), Some (Value.Float r) -> Some (Value.Float (l /. r))
  | _ -> raise (Lox_error.Runtime (op, "Operands must be numbers"))

let apply_star op l r =
  match (l, r) with
  | Some (Value.Float l), Some (Value.Float r) -> Some (Value.Float (l *. r))
  | _ -> raise (Lox_error.Runtime (op, "Operands must be numbers"))

let apply_plus op l r =
  match (l, r) with
  | Some (Value.Float l), Some (Value.Float r) -> Some (Value.Float (l +. r))
  | Some (Value.String l), Some (Value.String r) -> Some (Value.String (l ^ r))
  | _ ->
      raise
        (Lox_error.Runtime
           (op, "Operands must be either two numbers or two strings"))

let apply_greater op l r =
  match (l, r) with
  | Some (Value.Float l), Some (Value.Float r) -> Some (Value.Bool (l > r))
  | _ -> raise (Lox_error.Runtime (op, "Operands must be numbers"))

let apply_greater_equal op l r =
  match (l, r) with
  | Some (Value.Float l), Some (Value.Float r) -> Some (Value.Bool (l >= r))
  | _ -> raise (Lox_error.Runtime (op, "Operands must be numbers"))

let apply_less op l r =
  match (l, r) with
  | Some (Value.Float l), Some (Value.Float r) -> Some (Value.Bool (l < r))
  | _ -> raise (Lox_error.Runtime (op, "Operands must be numbers"))

let apply_less_equal op l r =
  match (l, r) with
  | Some (Value.Float l), Some (Value.Float r) -> Some (Value.Bool (l <= r))
  | _ -> raise (Lox_error.Runtime (op, "Operands must be numbers"))

let apply_binary left right (op : Token.t) =
  match (op.token_type, left, right) with
  | Token.MINUS, l, r -> apply_minus op l r
  | Token.SLASH, l, r -> apply_slash op l r
  | Token.STAR, l, r -> apply_star op l r
  | Token.PLUS, l, r -> apply_plus op l r
  | Token.GREATER, l, r -> apply_greater op l r
  | Token.GREATER_EQUAL, l, r -> apply_greater_equal op l r
  | Token.LESS, l, r -> apply_less op l r
  | Token.LESS_EQUAL, l, r -> apply_less_equal op l r
  | Token.BANG_EQUAL, l, r -> Some (Value.Bool (l <> r))
  | Token.EQUAL_EQUAL, l, r -> Some (Value.Bool (l = r))
  | _ -> failwith "unimplemented"

let rec evaluate env expr =
  match expr with
  | Expression.Literal value -> value
  | Grouping expr -> evaluate env expr
  | Unary (token, expr) ->
      let right = evaluate env expr in
      apply_unary token right
  | Binary (left, op, right) ->
      let left_val = evaluate env left in
      let right_val = evaluate env right in
      apply_binary left_val right_val op
  | Variable { token_type = Token.IDENTIFIER name; line } ->
      let result = Environment.get env name in
      Result.fold ~ok:Fun.id
        ~error:(fun err ->
          raise
            (Lox_error.Runtime
               ({ token_type = Token.IDENTIFIER name; line }, err)))
        result
  | Variable token ->
      (* This case should be impossible - parsing will never make a Variable token
         not holding an identifier *)
      raise
        (Lox_error.Runtime
           (token, "Invalid variable access, expected identifier."))

let execute env statement =
  match statement with
  | Statement.Expression expr ->
      let _ = evaluate env expr in
      env
  | Print expr ->
      let value = evaluate env expr in
      let _ = Printf.printf "%s\n%!" (Value.to_string value) in
      env
  | Var ({ token_type = Token.IDENTIFIER name; line = _ }, expr) ->
      let value = Option.bind expr (evaluate env) in
      Environment.define env name value
  | Var (token, _) ->
      (* This case should be impossible - use of an invalid, non-identifier token in
         a variable declaration is detected during parsing *)
      raise
        (Lox_error.Runtime
           (token, "Invalid var declaration, expected identifier."))

let interpret statements env =
  try
    let env =
      List.fold_left (fun curr_env s -> execute curr_env s) env statements
    in
    Ok env
  with Lox_error.Runtime (token, message) ->
    Lox_error.init_runtime token message
