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
           | `Right t | `Both (_, t) -> (
               match apply substitution (MonoType t) with
               | MonoType t -> Some t
               | _ -> failwith "Invalid substitution application")))
  | PolyType t -> PolyType (apply_polytype_substitution substitution t)
  | MonoType t -> MonoType (apply_monotype_substitution substitution t)

let rec free_vars_in_mono_type = function
  | Types.TypeVar v -> Set.singleton (module String) v
  | Types.TypeFunctionApplication fn -> (
      match fn with
      | Types.Arrow mus ->
          Set.union_list
            (module String)
            (List.map mus ~f:free_vars_in_mono_type)
      | _ -> Set.empty (module String))

and free_vars_in_poly_type = function
  | Types.MonoType t -> free_vars_in_mono_type t
  | Types.Quantified (t_var, poly_t) ->
      Set.diff
        (free_vars_in_poly_type poly_t)
        (Set.singleton (module String) t_var)

let rec free_vars_in_ctx ctx =
  Map.fold ctx
    ~init:(Set.empty (module String))
    ~f:(fun ~key:_ ~data:t free_vars ->
      Set.union free_vars (free_vars_in_poly_type t))

let generalize ctx mono_type =
  let quantifiers =
    Set.diff (free_vars_in_mono_type mono_type) (free_vars_in_ctx ctx)
  in
  Set.fold quantifiers ~init:(Types.MonoType mono_type) ~f:(fun acc t_var ->
      Types.Quantified (t_var, acc))

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
