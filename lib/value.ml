type t = String of string | Float of float | Bool of bool | Function of string

let to_string = function
  | Some (String s) -> s
  | Some (Float f) ->
      string_of_float f (* TODO: handle parsing out the . from doubles here *)
  | Some (Bool b) -> string_of_bool b
  | Some (Function s) -> "Fn: " ^ s
  | None -> "nil"
