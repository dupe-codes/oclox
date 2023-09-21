type t

val init : unit -> t
val get : t -> string -> (Value.t option, string) result
val define : t -> string -> Value.t option -> t
val assign : t -> string -> Value.t option -> (t, string) result
val print : t -> unit
