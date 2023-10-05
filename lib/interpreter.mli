val interpret :
  Statement.t list ->
  Environment.t ->
  Resolver.t ->
  (unit, Lox_error.error_type) result
