exception RuntimeError of Token.t * string

let apply_bang value =
  match value with
  | Some (Value.Bool x) -> Some (Value.Bool (not x))
  | None -> Some (Value.Bool true)
  | _ -> Some (Value.Bool false)

let apply_unary (token : Token.t) value =
  match (token.token_type, value) with
  | Token.MINUS, Some (Value.Float x) -> Some (Value.Float (-1. *. x))
  | Token.MINUS, _ -> raise (RuntimeError (token, "Operand must be a number"))
  | Token.BANG, v -> apply_bang v
  | _ -> failwith "Unimplemented unary operator"

let apply_minus op l r =
  match (l, r) with
  | Some (Value.Float l), Some (Value.Float r) -> Some (Value.Float (l -. r))
  | _ -> raise (RuntimeError (op, "Operands must be numbers"))

let apply_slash op l r =
  match (l, r) with
  | Some (Value.Float l), Some (Value.Float r) -> Some (Value.Float (l /. r))
  | _ -> raise (RuntimeError (op, "Operands must be numbers"))

let apply_star op l r =
  match (l, r) with
  | Some (Value.Float l), Some (Value.Float r) -> Some (Value.Float (l *. r))
  | _ -> raise (RuntimeError (op, "Operands must be numbers"))

let apply_plus op l r =
  match (l, r) with
  | Some (Value.Float l), Some (Value.Float r) -> Some (Value.Float (l +. r))
  | Some (Value.String l), Some (Value.String r) -> Some (Value.String (l ^ r))
  | _ ->
      raise
        (RuntimeError (op, "Operands must be either two numbers or two strings"))

let apply_greater op l r =
  match (l, r) with
  | Some (Value.Float l), Some (Value.Float r) -> Some (Value.Bool (l > r))
  | _ -> raise (RuntimeError (op, "Operands must be numbers"))

let apply_greater_equal op l r =
  match (l, r) with
  | Some (Value.Float l), Some (Value.Float r) -> Some (Value.Bool (l >= r))
  | _ -> raise (RuntimeError (op, "Operands must be numbers"))

let apply_less op l r =
  match (l, r) with
  | Some (Value.Float l), Some (Value.Float r) -> Some (Value.Bool (l < r))
  | _ -> raise (RuntimeError (op, "Operands must be numbers"))

let apply_less_equal op l r =
  match (l, r) with
  | Some (Value.Float l), Some (Value.Float r) -> Some (Value.Bool (l <= r))
  | _ -> raise (RuntimeError (op, "Operands must be numbers"))

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

let rec evaluate expr =
  match expr with
  | Expression.Literal value -> value
  | Grouping expr -> evaluate expr
  | Unary (token, expr) ->
      let right = evaluate expr in
      apply_unary token right
  | Binary (left, op, right) ->
      let left_val = evaluate left in
      let right_val = evaluate right in
      apply_binary left_val right_val op

let interpret expr =
  try
    match evaluate expr with
    | Some (Value.Float x) ->
        let _ = Printf.printf "%s\n%!" (string_of_float x) in
        Ok ()
    | Some (Value.Bool b) ->
        let _ = Printf.printf "%s\n%!" (string_of_bool b) in
        Ok ()
    | Some (Value.String s) ->
        let _ = Printf.printf "%s\n%!" s in
        Ok ()
    | None ->
        let _ = Printf.printf "nil\n%!" in
        Ok ()
  with RuntimeError (token, message) -> Error.init_runtime token message
