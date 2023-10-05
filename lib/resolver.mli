type t

val init : unit -> t
val resolve : Statement.t list -> t -> t
val get_resolved_var_depth : t -> Expression.t -> int option
val resolved_locals_to_string : t -> string
