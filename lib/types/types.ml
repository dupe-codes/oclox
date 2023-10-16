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
type type_var = string [@@deriving show]

type type_constructor = Arrow of mono_type list | Int | Bool [@@deriving show]

and mono_type = TypeVar of type_var | TypeConstructor of type_constructor
[@@deriving show]

type poly_type =
  | MonoType of mono_type
  | UniversallyQuantified of type_var * poly_type
[@@deriving show]

let create_new_type_var =
  let counter = ref 0 in
  fun () ->
    let new_var = Printf.sprintf "'t%d" !counter in
    counter := !counter + 1;
    new_var
