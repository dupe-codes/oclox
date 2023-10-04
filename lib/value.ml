open Ppx_compare_lib.Builtin
open Sexplib.Std

type function_type = { name : string; arity : int } [@@deriving compare, sexp]

and native_function = {
  function_type : function_type;
  fn : t option list -> t option; [@compare.ignore]
}
[@@deriving compare, sexp]

and t =
  | String of string
  | Float of float
  | Bool of bool
  | Function of function_type
  | Native of native_function
[@@deriving compare, sexp]

let to_string = function
  | Some (String s) -> s
  | Some (Float f) ->
      string_of_float f (* TODO: handle parsing out the . from doubles here *)
  | Some (Bool b) -> string_of_bool b
  | Some (Function { name; _ }) -> "Fn: " ^ name
  | Some (Native { function_type = { name; _ }; _ }) -> "Native fn: " ^ name
  | None -> "nil"
