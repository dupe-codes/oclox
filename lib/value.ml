type function_type = { name : string; arity : int }

type t =
  | String of string
  | Float of float
  | Bool of bool
  | Function of function_type
  | Native of function_type * (t option list -> t option)

let to_string = function
  | Some (String s) -> s
  | Some (Float f) ->
      string_of_float f (* TODO: handle parsing out the . from doubles here *)
  | Some (Bool b) -> string_of_bool b
  | Some (Function { name; _ }) -> "Fn: " ^ name
  | Some (Native ({ name; _ }, _)) -> "Native fn: " ^ name
  | None -> "nil"
