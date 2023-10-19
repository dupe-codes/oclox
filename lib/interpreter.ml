exception Return of Value.t option

(* TODO: Consider this type encapsulating the data that needs to be passed
   to all eval/execute functions.
   Or, this might be where storing data on the AST nodes is a better fit *)
(*type t = {*)
(*env : Environment.t;*)
(*resolver : Resolver.t;*)
(*(* ... Any results of further static analysis passes ... *)*)
(*}*)

let is_truthy value =
  match value with Some (Value.Bool x) -> x | None -> false | _ -> true

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

let check_arity_mismatch paren args arity =
  if List.length args != arity then
    raise
      (Lox_error.Runtime
         ( paren,
           Printf.sprintf "Expected %d arguments but got %d." arity
             (List.length args) ))

let rec evaluate env resolver expr =
  match expr with
  | Expression.Literal value -> value
  | Grouping expr -> evaluate env resolver expr
  | Unary (token, expr) ->
      let right = evaluate env resolver expr in
      apply_unary token right
  | Binary (left, op, right) ->
      let left_val = evaluate env resolver left in
      let right_val = evaluate env resolver right in
      apply_binary left_val right_val op
  | Variable ({ token_type = Token.IDENTIFIER name; line }, _) ->
      let distance = Resolver.get_resolved_var_depth resolver expr in
      let result =
        if Option.is_none distance then Environment.get env name
        else Environment.get_at env name (Option.get distance)
      in
      Result.fold ~ok:Fun.id
        ~error:(fun err ->
          raise
            (Lox_error.Runtime
               ({ token_type = Token.IDENTIFIER name; line }, err)))
        result
  | Logical (left, op, right) ->
      let left_result = evaluate env resolver left in
      let left_truthy = is_truthy left_result in
      if
        (op.token_type = Token.OR && left_truthy)
        || (op.token_type = Token.AND && not left_truthy)
      then left_result
      else evaluate env resolver right
  | Call (callee, paren, args) ->
      evaluate_fn_call callee paren args env resolver
  | Assign ({ token_type = Token.IDENTIFIER name; line }, expr, _) ->
      let value = evaluate env resolver expr in
      let distance = Resolver.get_resolved_var_depth resolver expr in
      let result =
        if Option.is_none distance then Environment.assign env name value
        else Environment.assign_at env name (Option.get distance) value
      in
      Result.fold
        ~ok:(fun _ -> value)
        ~error:(fun err ->
          raise
            (Lox_error.Runtime
               ({ token_type = Token.IDENTIFIER name; line }, err)))
        result
  | Variable (token, _) ->
      (* This case should be impossible - parsing will never make a
         Variable token not holding an identifier *)
      raise
        (Lox_error.Runtime
           (token, "Invalid variable access, expected identifier."))
  | Assign (token, _, _) ->
      (* This case should be impossible - parsing will never make a
         Variable token not holding an identifier *)
      raise
        (Lox_error.Runtime
           (token, "Invalid variable assignment, expected identifier."))

and evaluate_fn_call callee paren args env resolver =
  let callee_value = evaluate env resolver callee in
  let args =
    List.rev
      (List.fold_left
         (fun args a ->
           let arg = evaluate env resolver a in
           arg :: args)
         [] args)
  in
  match callee_value with
  | Some (Value.Function fn_def) ->
      let distance = Resolver.get_resolved_var_depth resolver callee in
      apply_function env resolver distance paren args fn_def
  | Some (Value.Native { function_type = fn_def; fn }) ->
      let _ = check_arity_mismatch paren args fn_def.arity in
      fn args
  | _ ->
      raise (Lox_error.Runtime (paren, "Can only call functions and classes"))

and apply_function env resolver distance paren args fn_def =
  let _ = check_arity_mismatch paren args fn_def.arity in
  let closed_fn =
    if Option.is_none distance then Environment.get_fn env fn_def.name
    else Environment.get_fn_at env fn_def.name (Option.get distance)
  in
  let args_env = Environment.with_enclosing closed_fn.closure in
  let _ =
    List.map
      (fun (param, arg) ->
        Environment.define args_env (Token.get_identifier_name param) arg)
      (List.combine closed_fn.fn.params args)
  in
  try
    let _ = execute_block args_env resolver closed_fn.fn.body in
    None
  with Return value -> value

and execute_block env resolver statements =
  let _ =
    List.map (fun statement -> execute env resolver statement) statements
  in
  ()

and evaluate_if env resolver condition then_branch else_branch =
  let condition_result = evaluate env resolver condition in
  match condition_result with
  | Some (Value.Bool true) -> execute env resolver then_branch
  | Some (Value.Bool false) ->
      Option.fold ~none:() ~some:(execute env resolver) else_branch
  | Some _ -> execute env resolver then_branch
  | None -> ()

and execute_while env resolver condition body =
  let rec loop env =
    let condition_result = evaluate env resolver condition in
    if is_truthy condition_result then
      let _ = execute env resolver body in
      loop env
  in
  loop env

and execute env resolver statement =
  match statement with
  | Statement.Expression expr ->
      let _ = evaluate env resolver expr in
      ()
  | Print expr ->
      let value = evaluate env resolver expr in
      Printf.printf "%s\n%!" (Value.to_string value)
  | Block statements ->
      let block_scope = Environment.with_enclosing env in
      execute_block block_scope resolver statements
  | If (condition, then_branch, else_branch) ->
      evaluate_if env resolver condition then_branch else_branch
  | While (condition, body) -> execute_while env resolver condition body
  | Function declaration ->
      Environment.define_fn env { fn = declaration; closure = env }
  | Return (_, expr) ->
      (*TODO: Brainstorm an alternative to this exceptions-as-control-flow approach*)
      let value = Option.fold ~none:None ~some:(evaluate env resolver) expr in
      raise (Return value)
  | Var ({ token_type = Token.IDENTIFIER name; line = _ }, expr) ->
      let value = Option.fold ~none:None ~some:(evaluate env resolver) expr in
      Environment.define env name value
  | Var (token, _) ->
      (* This case should be impossible - use of an invalid, non-identifier
         token in a variable declaration is detected as an error during
         parsing *)
      raise
        (Lox_error.Runtime
           (token, "Invalid var declaration, expected identifier."))

let interpret statements env resolver =
  try
    let execute_fn = execute env resolver in
    let _ = List.map execute_fn statements in
    Ok ()
  with Lox_error.Runtime (token, message) ->
    Lox_error.init_runtime token message
