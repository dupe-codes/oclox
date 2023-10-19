open Base

type context = Types.poly_type Map.M(String).t

let init_context (mappings : (string * Types.poly_type) list) =
  Map.of_alist_exn (module String) mappings

type substitution = Types.mono_type Map.M(String).t

let init_substitution (mappings : (string * Types.mono_type) list) =
  Map.of_alist_exn (module String) mappings

let print_context fmt context =
  let pp_item fmt (key, data) =
    Stdlib.Format.fprintf fmt "%s : " key;
    Types.print_poly_type fmt data
  in
  Map.to_alist context
  |> List.iter ~f:(fun item ->
         pp_item fmt item;
         Stdlib.Format.fprintf fmt "; ")

let print_substitution fmt substitution =
  let pp_item fmt (key, data) =
    Stdlib.Format.fprintf fmt "%s => " key;
    Types.print_mono_type fmt data
  in
  (* Convert the substitution map to a list and print each item *)
  Map.to_alist substitution
  |> List.iter ~f:(fun item ->
         pp_item fmt item;
         Stdlib.Format.fprintf fmt "; ")

type substitution_target =
  | Context of context
  | Substitution of substitution
  | PolyType of Types.poly_type
  | MonoType of Types.mono_type

let rec apply_monotype_substitution substitution = function
  | Types.TypeVar v -> (
      match Map.find substitution v with Some t -> t | None -> Types.TypeVar v)
  | TypeFunctionApplication type_function ->
      TypeFunctionApplication
        (match type_function with
        | Arrow mus ->
            Arrow (List.map mus ~f:(apply_monotype_substitution substitution))
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
  | TypeFunctionApplication fn -> (
      match fn with
      | Arrow mus ->
          Set.union_list
            (module String)
            (List.map mus ~f:free_vars_in_mono_type)
      | _ -> Set.empty (module String))

and free_vars_in_poly_type = function
  | Types.MonoType t -> free_vars_in_mono_type t
  | Quantified (t_var, poly_t) ->
      Set.diff
        (free_vars_in_poly_type poly_t)
        (Set.singleton (module String) t_var)

let free_vars_in_ctx ctx =
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

let rec contains type_a t_var =
  match type_a with
  | Types.TypeVar a -> String.equal a t_var
  | TypeFunctionApplication fn -> (
      match fn with
      | Arrow mus ->
          List.fold mus ~init:false ~f:(fun acc t -> acc || contains t t_var)
      | _ -> false)

let rec instantiate ?(mappings = Map.empty (module String)) poly_type =
  match poly_type with
  | Types.MonoType (TypeVar t_var) -> (
      match Map.find mappings t_var with
      | Some t -> t
      | _ -> Types.TypeVar t_var)
  | Types.MonoType (TypeFunctionApplication fn) -> (
      match fn with
      | Arrow mus ->
          TypeFunctionApplication
            (Arrow
               (List.map mus ~f:(fun mu ->
                    instantiate ~mappings (Types.MonoType mu))))
      | _ -> Types.TypeFunctionApplication fn)
  | Types.Quantified (q_var, sigma) ->
      let fresh_var = Types.TypeVar (Types.create_new_type_var ()) in
      instantiate ~mappings:(Map.set mappings ~key:q_var ~data:fresh_var) sigma

exception UnificationFailure of string

let rec unify type_a type_b =
  match (type_a, type_b) with
  | Types.TypeVar a, Types.TypeVar b when String.equal a b ->
      init_substitution []
  | TypeVar a, t ->
      if contains t a then raise (UnificationFailure "Infinite type")
      else init_substitution [ (a, t) ]
  | _, TypeVar _ -> unify type_b type_a
  | TypeFunctionApplication a, TypeFunctionApplication b ->
      unify_type_functions a b

and unify_type_functions fn_a fn_b =
  match (fn_a, fn_b) with
  | Types.Float, Types.Float | Bool, Bool | Unit, Unit | String, String ->
      init_substitution []
  | Arrow a_mus, Arrow b_mus when List.length a_mus <> List.length b_mus ->
      raise
        (UnificationFailure
           "Could not unify type functions of differing lengths")
  | Arrow a_mus, Arrow b_mus ->
      let zipped = List.zip_exn a_mus b_mus in
      List.fold ~init:(init_substitution []) ~f:unify_substitutions zipped
  | _ ->
      raise
        (UnificationFailure "Could not unify types: differing type functions")

and unify_substitutions subs (type_a, type_b) =
  match (apply subs (MonoType type_a), apply subs (MonoType type_b)) with
  | MonoType subbed_a, MonoType subbed_b -> (
      match apply subs (Substitution (unify subbed_a subbed_b)) with
      | Substitution s -> s
      | _ -> failwith "Invalid substitution application")
  | _ -> failwith "Invalid substitution application"

let apply_sub_to_context sub ctx =
  match apply sub (Context ctx) with
  | Context ctx -> ctx
  | _ -> failwith "Invalid substitution application"

let apply_sub_to_sub sub1 sub2 =
  match apply sub1 (Substitution sub2) with
  | Substitution sub -> sub
  | _ -> failwith "Invalid substitution application"

let apply_sub_to_type sub t =
  match apply sub (MonoType t) with
  | MonoType t -> t
  | _ -> failwith "Invalid substitution application"
