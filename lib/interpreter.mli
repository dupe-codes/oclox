val interpret :
  Statement.t list -> Environment.t -> (unit, Lox_error.error_type) result
