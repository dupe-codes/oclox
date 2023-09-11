type t

val init : Token.t list -> t
(* Expects that the last token in the given token list is Token.EOF *)

val parse : t -> Statement.t list option
