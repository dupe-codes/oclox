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
  | Binary (left, op, right) | Logical (left, op, right) ->
      infer_binary_op_types ctx left op right
  | Grouping e -> infer_expr_type ctx e
  | Variable (x, _) -> (
      let var = Token.get_identifier_name x in
      match Map.find ctx var with
      | Some t -> (empty_sub, instantiate t)
      | None -> raise (UnificationFailure ("Undefined variable " ^ var)))
  | Call (callee, _, params) ->
      (* TODO: For now, only handle functions with one parameter
         To handle multiple: iteratre over params list
         TO handle none: use unit type as a single "parameter"
      *)
      let callee_sub, callee_type = infer_expr_type ctx callee in
      (*let _ =*)
      (*Stdlib.Printf.printf "\n Callee type: \n";*)
      (*Types.print_mono_types [ callee_type ]*)
      (*in*)
      let params_sub, param_types =
        if List.is_empty params then
          (init_substitution [], [ Types.TypeFunctionApplication Unit ])
        else
          List.fold params
            ~init:(init_substitution [], [])
            ~f:(fun (sub, types) e ->
              (* TODO: Do we need to propagate the context too?
                 Or is it reflected in the accumulating substitution? *)
              let param_sub, param_type =
                infer_expr_type (apply_sub_to_context sub ctx) e
              in
              (apply_sub_to_sub param_sub sub, param_type :: types))
      in
      (*let _ =*)
      (*Stdlib.Printf.printf "\n Param types: \n";*)
      (*Types.print_mono_types (List.rev param_types)*)
      (*in*)
      let return_t_var = Types.TypeVar (Types.create_new_type_var ()) in
      let return_sub =
        unify
          (apply_sub_to_type params_sub callee_type)
          (Types.TypeFunctionApplication
             (Arrow (List.rev param_types @ [ return_t_var ])))
      in
      ( apply_sub_to_sub params_sub callee_sub |> apply_sub_to_sub return_sub,
        apply_sub_to_type return_sub return_t_var )
  | Unary (_, _) -> failwith "TODO"
  | Assign (_, _, _) -> failwith "TODO"

and infer_binary_op_types ctx left op right =
  (* Treat binary expressions as a special case of function applications *)
  let left_sub, left_type = infer_expr_type ctx left in
  let right_sub, right_type =
    infer_expr_type (apply_sub_to_context left_sub ctx) right
  in
  let op_type = Token.type_to_string op.token_type |> Map.find_exn ctx in
  match op_type with
  | Types.MonoType
      (TypeFunctionApplication (Arrow [ param1; param2; return_type ]))
  | Types.Quantified
      ( _,
        Types.Quantified
          ( _,
            Types.MonoType
              (TypeFunctionApplication (Arrow [ param1; param2; return_type ]))
          ) ) ->
      let unify_sub1 = unify param1 left_type in
      let unify_sub2 = unify param2 right_type in
      let final_sub =
        apply_sub_to_sub right_sub left_sub
        |> apply_sub_to_sub unify_sub1
        |> apply_sub_to_sub unify_sub2
      in
      (final_sub, apply_sub_to_type final_sub return_type)
  | _ ->
      raise
        (UnificationFailure
           ("Mismatched types for binary operator " ^ Token.to_string op))

let rec infer_stmt_type (ctx, types) stmt =
  let unit_type = Types.MonoType (TypeFunctionApplication Unit) in
  match stmt with
  | Statement.Expression e ->
      let sub, e_type = infer_expr_type ctx e in
      (apply_sub_to_context sub ctx, Types.MonoType e_type :: types)
  | Return (_, e) -> (
      match e with
      | Some e ->
          let sub, e_type = infer_expr_type ctx e in
          (apply_sub_to_context sub ctx, Types.MonoType e_type :: types)
      | None -> (ctx, unit_type :: types))
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
  | Block stmts ->
      let _ =
        List.iter stmts ~f:(fun stmt ->
            let _ = infer_stmt_type (ctx, types) stmt in
            ())
      in
      (ctx, unit_type :: types)
  | Print e ->
      let _ = infer_expr_type ctx e in
      (ctx, types)
  (* TODO: type check sub components of each stmt *)
  | If _ | While _ -> (ctx, unit_type :: types)

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
     them in the function signature
     TODO: Only remove from context if v is a value for one of the params! You fool!
     Other things could still be TypeVars! *)
  let fn_ctx =
    Map.filter
      ~f:(fun v ->
        match v with Types.MonoType (TypeVar _) -> false | _ -> true)
      body_ctx
  in
  let final_param_types =
    if List.is_empty subbed_param_types then
      [ Types.TypeFunctionApplication Unit ]
    else subbed_param_types
  in
  let fn_type =
    generalize fn_ctx
      (Types.TypeFunctionApplication
         (Arrow (final_param_types @ [ instantiate return_type ])))
  in
  let new_ctx = Map.set fn_ctx ~key:fn_name ~data:fn_type in
  (new_ctx, fn_type)

(* Output types *)
type typed_statement = |
type typed_expr = |

(* TODO: Make PLUS polymorphic but constrained to Float and String *)
let float_binary_ops =
  [
    "PLUS";
    "MINUS";
    "SLASH";
    "STAR";
    "GREATER";
    "GREATER_EQUAL";
    "LESS";
    "LESS_EQUAL";
  ]

let polymorphic_binary_ops = [ "EQUAL_EQUAL"; "BANG_EQUAL" ]

(* TODO: Handle the fact that OR and AND accept "truthy" values;
   e.g., 1 and (x == 2); is valid *)
let boolean_ops = [ "OR"; "AND" ]

let builtins_context =
  init_context
    (List.map float_binary_ops ~f:(fun op ->
         ( op,
           Types.MonoType
             (TypeFunctionApplication
                (Arrow
                   [
                     TypeFunctionApplication Float;
                     TypeFunctionApplication Float;
                     TypeFunctionApplication Float;
                   ])) ))
    @ List.map polymorphic_binary_ops ~f:(fun op ->
          let type_var1 = Types.create_new_type_var () in
          let type_var2 = Types.create_new_type_var () in
          ( op,
            Types.Quantified
              ( type_var1,
                Quantified
                  ( type_var2,
                    MonoType
                      (TypeFunctionApplication
                         (Arrow
                            [
                              TypeVar type_var1;
                              TypeVar type_var2;
                              TypeFunctionApplication Bool;
                            ])) ) ) ))
    @ List.map boolean_ops ~f:(fun op ->
          ( op,
            Types.MonoType
              (TypeFunctionApplication
                 (Arrow
                    [
                      TypeFunctionApplication Bool;
                      TypeFunctionApplication Bool;
                      TypeFunctionApplication Bool;
                    ])) )))

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
  (*let _ =*)
  (*Stdlib.Format.printf "@[<v 2>Context: @,%a@,@]" Substitutions.print_context*)
  (*builtins_context*)
  (*in*)
  let initial_state = (builtins_context, []) in
  let _, inferred_types =
    List.fold_left ~f:infer_stmt_type ~init:initial_state stmts
  in
  List.rev inferred_types
