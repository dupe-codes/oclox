type t

val init : unit -> t
val global : unit -> t
val with_enclosing : t -> t
val get_enclosing : t -> t option
val get : t -> string -> (Value.t option, string) result
val get_fn : t -> string -> Statement.fn_declaration
val define : t -> string -> Value.t option -> t
val define_fn : t -> Statement.fn_declaration -> t
val assign : t -> string -> Value.t option -> (t, string) result
val print : t -> unit
