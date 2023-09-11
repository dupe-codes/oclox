type t

val init : string -> t
val scan_tokens : t -> (Token.t list, Error.error_type) result
