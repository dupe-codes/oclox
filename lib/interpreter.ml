exception Return of Value.t option

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

let rec evaluate env expr =
  match expr with
  | Expression.Literal value -> (env, value)
  | Grouping expr -> evaluate env expr
  | Unary (token, expr) ->
      let env, right = evaluate env expr in
      (env, apply_unary token right)
  | Binary (left, op, right) ->
      let env, left_val = evaluate env left in
      let env, right_val = evaluate env right in
      (env, apply_binary left_val right_val op)
  | Variable { token_type = Token.IDENTIFIER name; line } ->
      let result = Environment.get env name in
      Result.fold
        ~ok:(fun value -> (env, value))
        ~error:(fun err ->
          raise
            (Lox_error.Runtime
               ({ token_type = Token.IDENTIFIER name; line }, err)))
        result
  | Logical (left, op, right) ->
      let env, left_result = evaluate env left in
      let left_truthy = is_truthy left_result in
      if
        (op.token_type = Token.OR && left_truthy)
        || (op.token_type = Token.AND && not left_truthy)
      then (env, left_result)
      else evaluate env right
  | Call (callee, paren, args) -> evaluate_fn_call callee paren args env
  | Assign ({ token_type = Token.IDENTIFIER name; line }, expr) ->
      let env, value = evaluate env expr in
      let result = Environment.assign env name value in
      Result.fold
        ~ok:(fun env -> (env, value))
        ~error:(fun err ->
          raise
            (Lox_error.Runtime
               ({ token_type = Token.IDENTIFIER name; line }, err)))
        result
  | Variable token ->
      (* This case should be impossible - parsing will never make a
         Variable token not holding an identifier *)
      raise
        (Lox_error.Runtime
           (token, "Invalid variable access, expected identifier."))
  | Assign (token, _) ->
      (* This case should be impossible - parsing will never make a
         Variable token not holding an identifier *)
      raise
        (Lox_error.Runtime
           (token, "Invalid variable assignment, expected identifier."))

and evaluate_fn_call callee paren args env =
  let env, callee = evaluate env callee in
  let env, args =
    List.fold_left
      (fun (env, args) a ->
        let new_env, arg = evaluate env a in
        (new_env, arg :: args))
      (env, []) args
  in
  let args = List.rev args in
  match callee with
  | Some (Value.Function fn_def) -> apply_function env paren args fn_def
  | Some (Value.Native (fn_def, fn)) ->
      let _ = check_arity_mismatch paren args fn_def.arity in
      (env, fn args)
  | _ ->
      raise (Lox_error.Runtime (paren, "Can only call functions and classes"))

and apply_function env paren args fn_def =
  let _ = check_arity_mismatch paren args fn_def.arity in
  let fn_declaration = Environment.get_fn env fn_def.name in
  let args_env = Environment.with_enclosing env in
  let args_env =
    List.fold_left
      (fun env (param, arg) ->
        Environment.define env (Token.get_identifier_name param) arg)
      args_env
      (List.combine fn_declaration.params args)
  in
  try
    execute_block args_env fn_declaration.body |> fun (env, res) ->
    (Option.get (Environment.get_enclosing env), res)
  with Return value ->
    (* FIXME: This is incorrect - the enclosing env from args_env will _not_ contain
        any mutations that may have happened in the execution of the function block

       My idea: in the returned exception, pass back up the environment to return.
       To know which to use in the linked list of environments, we can pass an arg
       counting the block depth from here in execute block, and use that to index
       back in env linked list when throwing the return exception

       We can pass this depth count in the env itself (int option). If one is set in
       an env, every call to Environment#with_enclosing will increment the count,
       and Environment#get_enclosing will decrement it
    *)
    (Option.get (Environment.get_enclosing args_env), value)

and execute_block env statements =
  let block_scope = Environment.with_enclosing env in
  let env =
    List.fold_left (fun curr_env s -> execute curr_env s) block_scope statements
  in
  (Option.get (Environment.get_enclosing env), None)

and evaluate_if env condition then_branch else_branch =
  let env, condition_result = evaluate env condition in
  match condition_result with
  | Some (Value.Bool true) -> execute env then_branch
  | Some (Value.Bool false) ->
      Option.fold ~none:env ~some:(execute env) else_branch
  | Some _ -> execute env then_branch
  | None -> env

and execute_while env condition body =
  let rec loop env =
    let env, condition_result = evaluate env condition in
    if not (is_truthy condition_result) then env else loop (execute env body)
  in
  loop env

and execute env statement =
  match statement with
  | Statement.Expression expr ->
      let env, _ = evaluate env expr in
      env
  | Print expr ->
      let env, value = evaluate env expr in
      let _ = Printf.printf "%s\n%!" (Value.to_string value) in
      env
  | Block statements -> fst (execute_block env statements)
  | If (condition, then_branch, else_branch) ->
      evaluate_if env condition then_branch else_branch
  | While (condition, body) -> execute_while env condition body
  | Function declaration -> Environment.define_fn env declaration
  | Return (_, expr) ->
      (*TODO: Brainstorm an alternative to this exceptions-as-control-flow approach*)
      let _, value = Option.fold ~none:(env, None) ~some:(evaluate env) expr in
      raise (Return value)
  | Var ({ token_type = Token.IDENTIFIER name; line = _ }, expr) ->
      let env, value =
        Option.fold ~none:(env, None) ~some:(evaluate env) expr
      in
      Environment.define env name value
  | Var (token, _) ->
      (* This case should be impossible - use of an invalid, non-identifier
         token in a variable declaration is detected as an error during
         parsing *)
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
