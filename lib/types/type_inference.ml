(* Implementation of a rudimentary Hindler Milner type system *)

open Base

type context = Types.poly_type Map.M(String).t

let init_context (mappings : (string * Types.poly_type) list) =
  Map.of_alist_exn (module String) mappings

type substitution = Types.mono_type Map.M(String).t

let init_substitution (mappings : (string * Types.mono_type) list) =
  Map.of_alist_exn (module String) mappings

type substitution_target =
  | Context of context
  | Substitution of substitution
  | PolyType of Types.poly_type
  | MonoType of Types.mono_type

let rec apply_monotype_substitution substitution = function
  | Types.TypeVar v -> (
      match Map.find substitution v with Some t -> t | None -> Types.TypeVar v)
  | Types.TypeFunctionApplication type_function ->
      TypeFunctionApplication
        (match type_function with
        | Types.Arrow mus ->
            Types.Arrow
              (List.map mus ~f:(apply_monotype_substitution substitution))
        | t -> t)

and apply_polytype_substitution substitution = function
  | Types.MonoType t ->
      Types.MonoType (apply_monotype_substitution substitution t)
  | Quantified (t_var, poly_t) ->
      Quantified (t_var, apply_polytype_substitution substitution poly_t)

and apply substitution target =
  match target with
  | Context ctx ->
      Context
        (Map.map ctx ~f:(fun t ->
             match apply substitution (PolyType t) with
             | PolyType t -> t
             | _ -> failwith "Invalid substitution application"))
  | Substitution sub ->
      Substitution
        (Map.merge substitution sub ~f:(fun ~key:_ -> function
           | `Left t -> Some t
           | `Right t -> (
               match apply substitution (MonoType t) with
               | MonoType t -> Some t
               | _ -> failwith "Invalid substitution application")
           | `Both (_, t) -> (
               match apply substitution (MonoType t) with
               | MonoType t -> Some t
               | _ -> failwith "Invalid substitution application")))
  | PolyType t -> PolyType (apply_polytype_substitution substitution t)
  | MonoType t -> MonoType (apply_monotype_substitution substitution t)

(* TODO: Define method to instantiate type vars for quantified expressions,
        e.g., Va Vb a -> b => t0 -> t1 *)

(* TODO: Define generalize method that, given a context and a mono type, returns a
   universally quantified polytype *)

(* TODO: Diff function to get set difference of two free variables sets,
   and function to calculate free variables in a type or context *)

(* TODO: Function for unification of two types,
   mono_type -> mono_type -> substitution *)

(* Output types *)
type typed_statement = |
type typed_expr = |

(* TODO:
   - How do we handle context across a list of statements?
     - Intuitively, type assignments for names in the _global_ scope
       should propagate across inference of each statement.
*)
let infer (stmts : Statement.t list) =
  let (x : typed_statement list) = [] in
  x
