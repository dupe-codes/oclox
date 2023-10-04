open Ppx_compare_lib.Builtin
open Sexplib.Std

type t =
  | Binary of t * Token.t * t
  | Grouping of t
  | Literal of Value.t option
  | Unary of Token.t * t
  | Variable of Token.t
  | Assign of Token.t * t
  | Logical of t * Token.t * t
  | Call of t * Token.t * t list
[@@deriving compare, sexp]
