open Base

type typed_statement
type typed_expr
type context = Types.poly_type Map.M(String).t
type substitution = Types.mono_type Map.M(String).t

type substitution_target =
  | Context of context
  | Substitution of substitution
  | PolyType of Types.poly_type
  | MonoType of Types.mono_type

val init_context : (string * Types.poly_type) list -> context
val init_substitution : (string * Types.mono_type) list -> substitution
val apply : substitution -> substitution_target -> substitution_target

(* struct holding AST node and its inferred type *)

val infer : Statement.t list -> typed_statement list
