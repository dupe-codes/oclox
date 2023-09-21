type t =
  | Binary of t * Token.t * t
  | Grouping of t
  | Literal of Value.t option
  | Unary of Token.t * t
  | Variable of Token.t
  | Assign of Token.t * t
