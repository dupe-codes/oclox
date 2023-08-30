type t

val init : string -> t
val scan_tokens : t -> (Token.t list, string) result
