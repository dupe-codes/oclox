type t

val init : unit -> t
val global : unit -> t
val with_enclosing : t -> t
val get_enclosing : t -> t option
val get : t -> string -> (Value.t option, string) result
val define : t -> string -> Value.t option -> t
val assign : t -> string -> Value.t option -> (t, string) result
val print : t -> unit
