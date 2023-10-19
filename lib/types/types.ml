(*
 * Monotypes
 * ---------
 * mu ::= a
 *   |  C mu_0 ... mu_n
 *
 * where a is a type variable ('a, 'b, ...) and C is a function type constructor
 * such as 'a -> 'b or Int (no args)
 *
 * Polytypes
 * ---------
 * sigma ::= mu
 *        | Va. sigma
 *)
type type_var = string

(* TODO: Shorten names a bit *)
type type_function = Arrow of mono_type list | Float | Bool | String | Unit
and mono_type = TypeVar of type_var | TypeFunctionApplication of type_function

type poly_type = MonoType of mono_type | Quantified of type_var * poly_type

let mono_equal type1 type2 = type1 = type2

let create_new_type_var =
  let counter = ref 0 in
  fun () ->
    let new_var = Printf.sprintf "'t%d" !counter in
    counter := !counter + 1;
    new_var

(* Section: Printing Utilities *)

let rec print_type_function fmt = function
  | Float -> Format.fprintf fmt "Float"
  | String -> Format.fprintf fmt "String"
  | Bool -> Format.fprintf fmt "Bool"
  | Unit -> Format.fprintf fmt "()"
  | Arrow types ->
      let rec print_args = function
        | [] -> ()
        | [ last ] -> print_mono_type fmt last
        | arg :: rest ->
            print_mono_type fmt arg;
            Format.fprintf fmt " -> ";
            print_args rest
      in
      print_args types

and print_mono_type fmt = function
  | TypeVar var -> Format.fprintf fmt "%s" var
  | TypeFunctionApplication func -> print_type_function fmt func

and print_poly_type fmt = function
  | MonoType mono -> print_mono_type fmt mono
  | Quantified (type_var, poly) ->
      Format.fprintf fmt "∀ %s, " type_var;
      print_poly_type fmt poly

let print_types types =
  let _ = List.iter (Format.printf "%a\n" print_poly_type) types in
  Format.pp_print_flush Format.std_formatter ()
