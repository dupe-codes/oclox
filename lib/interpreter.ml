let apply_unary (token : Token.t) value =
  match (token.token_type, value) with
  | Token.MINUS, Some (Value.Float x) -> Some (Value.Float (-1. *. x))
  | _ -> failwith "unimplemented"

let apply_binary left right (op : Token.t) =
  match (op.token_type, left, right) with
  | Token.MINUS, Some (Value.Float l), Some (Value.Float r) ->
      Some (Value.Float (l -. r))
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
  match evaluate expr with
  | Some (Value.Float x) -> Printf.printf "%s\n%!" (string_of_float x)
  | _ -> failwith "unimplemented"
