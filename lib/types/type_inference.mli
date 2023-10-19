open Base

type typed_statement
type typed_expr

(* TODO: Eventually, return new typed statement struct, annotating all
   AST nodes with type info *)
val infer : Statement.t list -> Types.poly_type list
