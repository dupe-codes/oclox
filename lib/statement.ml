type t =
  | Block of t list
  | Expression of Expression.t
  | Print of Expression.t
  | Return of Token.t * Expression.t option
  | Var of Token.t * Expression.t option
  | If of Expression.t * t * t option
  | While of Expression.t * t
  | Function of fn_declaration

and fn_declaration = { name : Token.t; params : Token.t list; body : t list }
