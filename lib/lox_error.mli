type error_type = Syntax_error | Runtime_error

exception Runtime of Token.t * string

(* TODO: rename these to be more specific; e.g., init_syntax *)
val init : int -> string -> ('a, error_type) result
val init_for_token : Token.t -> string -> ('a, error_type) result
val init_runtime : Token.t -> string -> ('a, error_type) result
