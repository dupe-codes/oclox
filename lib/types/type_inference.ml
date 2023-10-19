(* Implementation of a rudimentary Hindler Milner type system *)

open Base
open Substitutions

let rec infer_expr_type ctx expr =
  let empty_sub = init_substitution [] in
  match expr with
  | Expression.Literal (Some (Float _)) ->
      (empty_sub, Types.TypeFunctionApplication Float)
  | Literal (Some (Bool _)) -> (empty_sub, Types.TypeFunctionApplication Bool)
  | Literal (Some (String _)) ->
      (empty_sub, Types.TypeFunctionApplication String)
  (* TODO: How to handle function literals? *)
  | Literal _ -> (empty_sub, Types.TypeFunctionApplication Unit)
  | Binary (left, op, right) -> (
      (* Treat binary expressions as a special case of function applications *)
      let left_sub, left_type = infer_expr_type ctx left in
      let right_sub, right_type =
        infer_expr_type (apply_sub_to_context left_sub ctx) right
      in
      let op_token = Token.type_to_string op.token_type in
      let op_type = Map.find_exn ctx op_token in
      match op_type with
      | Types.MonoType
          (TypeFunctionApplication (Arrow [ param1; param2; return_type ])) ->
          let unify_sub1 = unify param1 left_type in
          let unify_sub2 = unify param2 right_type in
          let final_sub =
            apply_sub_to_sub unify_sub2
              (apply_sub_to_sub unify_sub1
                 (apply_sub_to_sub right_sub left_sub))
          in
          (final_sub, apply_sub_to_type final_sub return_type)
      | _ ->
          raise
            (UnificationFailure
               ("Mismatched types for binary operator " ^ op_token)))
  | Grouping e -> infer_expr_type ctx e
  | Variable (x, _) -> (
      let var = Token.get_identifier_name x in
      match Map.find ctx var with
      | Some t -> (empty_sub, instantiate t)
      | None -> raise (UnificationFailure ("Undefined variable " ^ var)))
  | Call (callee, _, params) ->
      (* NOTE: For now, only handle functions with one parameter *)
      let callee_sub, callee_type = infer_expr_type ctx callee in
      let param_sub, param_type =
        infer_expr_type
          (apply_sub_to_context callee_sub ctx)
          (List.hd_exn params)
      in
      let new_t_var = Types.create_new_type_var () in
      let return_sub =
        unify
          (apply_sub_to_type param_sub callee_type)
          (Types.TypeFunctionApplication
             (Arrow [ param_type; TypeVar new_t_var ]))
      in
      ( apply_sub_to_sub return_sub (apply_sub_to_sub param_sub callee_sub),
        apply_sub_to_type return_sub (TypeVar new_t_var) )
  | _ -> failwith "TODO"

let rec infer_stmt_type (ctx, types) stmt =
  match stmt with
  | Statement.Expression e ->
      let sub, e_type = infer_expr_type ctx e in
      (apply_sub_to_context sub ctx, Types.MonoType e_type :: types)
  | Return (_, e) -> (
      match e with
      | Some e ->
          let sub, e_type = infer_expr_type ctx e in
          (apply_sub_to_context sub ctx, Types.MonoType e_type :: types)
      | None -> (ctx, Types.MonoType (TypeFunctionApplication Unit) :: types))
  | Var (x, e) ->
      let var_name = Token.get_identifier_name x in
      let sub, inferred_type =
        match e with
        (* Use unit type as a compromise for an uninitialized variable *)
        | None -> (init_substitution [], Types.TypeFunctionApplication Unit)
        | Some e -> infer_expr_type ctx e
      in
      let new_ctx = apply_sub_to_context sub ctx in
      let new_ctx =
        Map.set new_ctx ~key:var_name ~data:(generalize new_ctx inferred_type)
      in
      (new_ctx, Types.MonoType inferred_type :: types)
  | Function { name; params; body } ->
      let new_ctx, fn_type = infer_fn_type ctx name params body in
      (new_ctx, fn_type :: types)
  | _ -> failwith "TODO"

and infer_fn_type ctx name params body =
  let fn_name = Token.get_identifier_name name in
  let param_types =
    List.map params ~f:(fun _ -> Types.TypeVar (Types.create_new_type_var ()))
  in
  (* TODO: Also add a context entry for the fn sig, to handle recursion.
     E.g., name : t0 -> t1 *)
  let extended_ctx =
    List.fold2_exn params param_types ~init:ctx
      ~f:(fun acc_ctx param param_type ->
        let param_name = Token.get_identifier_name param in
        Map.set acc_ctx ~key:param_name ~data:(Types.MonoType param_type))
  in
  let body_ctx, body_types =
    List.fold_left body ~init:(extended_ctx, []) ~f:infer_stmt_type
  in
  (* Determine the return type - for simplicity, we take the type of the last statement *)
  (* TODO: Instead, find all return statements and their types, then unify them *)
  let return_type =
    match List.last body_types with
    | Some t -> t
    (* No explicit return, assume unit/void *)
    | None -> Types.MonoType (Types.TypeFunctionApplication Unit)
  in
  let subbed_param_types =
    List.map params ~f:(fun param ->
        instantiate (Map.find_exn body_ctx (Token.get_identifier_name param)))
  in
  (* Remove parameters that we not bound in the body context - we generalize over
     them in the function signature *)
  let fn_ctx =
    Map.filter
      ~f:(fun v ->
        match v with Types.MonoType (TypeVar _) -> false | _ -> true)
      body_ctx
  in
  let fn_type =
    generalize fn_ctx
      (Types.TypeFunctionApplication
         (Arrow (subbed_param_types @ [ instantiate return_type ])))
  in
  let new_ctx = Map.set fn_ctx ~key:fn_name ~data:fn_type in
  (new_ctx, fn_type)

(* Output types *)
type typed_statement = |
type typed_expr = |

let builtins_context =
  init_context
    [
      ( "PLUS",
        Types.MonoType
          (TypeFunctionApplication
             (Arrow
                [
                  TypeFunctionApplication Float;
                  TypeFunctionApplication Float;
                  TypeFunctionApplication Float;
                ])) );
    ]

(* TODO:
    - How do we handle context across a list of statements?
      - Intuitively, type assignments for names in the _global_ scope
        should propagate across inference of each statement.
   - Now, think about how to handle nested scopes. What's the interaction
     with type envs (contexts)?
*)
(* TODO: Pass in a type env context (optionally) to propagate type info in a
   REPL session *)
let infer (stmts : Statement.t list) =
  let initial_state = (builtins_context, []) in
  let _, inferred_types =
    List.fold_left ~f:infer_stmt_type ~init:initial_state stmts
  in
  List.rev inferred_types
