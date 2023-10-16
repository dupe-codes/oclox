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

type type_function = Arrow of mono_type list | Int | Bool | Unit
and mono_type = TypeVar of type_var | TypeFunctionApplication of type_function

type poly_type = MonoType of mono_type | Quantified of type_var * poly_type

let create_new_type_var =
  let counter = ref 0 in
  fun () ->
    let new_var = Printf.sprintf "'t%d" !counter in
    counter := !counter + 1;
    new_var
