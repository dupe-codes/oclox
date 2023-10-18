open Base

let rec print_type_function fmt = function
  | Types.Float -> Stdlib.Format.fprintf fmt "Float"
  | Types.String -> Stdlib.Format.fprintf fmt "String"
  | Types.Bool -> Stdlib.Format.fprintf fmt "Bool"
  | Types.Unit -> Stdlib.Format.fprintf fmt "()"
  | Types.Arrow types ->
      let rec print_args = function
        | [] -> ()
        | [ last ] -> print_mono_type fmt last
        | arg :: rest ->
            print_mono_type fmt arg;
            Stdlib.Format.fprintf fmt " -> ";
            print_args rest
      in
      print_args types

and print_mono_type fmt = function
  | Types.TypeVar var -> Stdlib.Format.fprintf fmt "%s" var
  | Types.TypeFunctionApplication func -> print_type_function fmt func

and print_poly_type fmt = function
  | Types.MonoType mono -> print_mono_type fmt mono
  | Types.Quantified (type_var, poly) ->
      Stdlib.Format.fprintf fmt "∀ %s, " type_var;
      print_poly_type fmt poly

let print_types types =
  let _ = List.iter types ~f:(Stdlib.Format.printf "%a\n" print_mono_type) in
  Stdlib.Format.pp_print_flush Stdlib.Format.std_formatter ()
