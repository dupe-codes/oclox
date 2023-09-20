type t

val init : string -> t
val scan_tokens : t -> (Token.t list, Lox_error.error_type) result
