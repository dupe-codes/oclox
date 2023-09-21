type t =
  | Block of t list
  | Expression of Expression.t
  | Print of Expression.t
  | Var of Token.t * Expression.t option
