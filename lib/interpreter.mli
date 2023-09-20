val interpret :
  Statement.t list ->
  Environment.t ->
  (Environment.t, Lox_error.error_type) result
