type t
type closed_function = { fn : Statement.fn_declaration; closure : t }

val init : unit -> t
val global : unit -> t
val with_enclosing : t -> t
val get_enclosing : t -> t option
val get : t -> string -> (Value.t option, string) result
val get_fn : t -> string -> closed_function
val define : t -> string -> Value.t option -> unit
val define_fn : t -> closed_function -> unit
val assign : t -> string -> Value.t option -> (unit, string) result
val print : t -> unit
