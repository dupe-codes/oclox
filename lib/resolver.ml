open Base

module ExprKey = struct
  (* Wrapper around Expression.t types for use as keys in the
     resolved variables Map *)
  module T = struct
    type t = Expression.t

    let compare = Expression.compare
    let sexp_of_t = Expression.sexp_of_t
  end

  include T
  include Comparator.Make (T)
end

type locals = int Map.M(ExprKey).t
type scopes = bool Map.M(String).t
type t = { scopes : scopes list; resolved_locals : locals }

let init () =
  {
    scopes = [ Map.empty (module String) ];
    resolved_locals = Map.empty (module ExprKey);
  }

let begin_scope resolver =
  { resolver with scopes = Map.empty (module String) :: resolver.scopes }

let end_scope resolver = { resolver with scopes = List.tl_exn resolver.scopes }

let update_top_scope name resolver fully_initialized =
  match resolver.scopes with
  | [] -> resolver
  | scope :: rest ->
      let updated_scope =
        Map.set scope
          ~key:(Token.get_identifier_name name)
          ~data:fully_initialized
      in
      { resolver with scopes = updated_scope :: rest }

let declare name resolver = update_top_scope name resolver false
let define name resolver = update_top_scope name resolver true

let resolve_local expr var resolver =
  let depth =
    List.findi resolver.scopes ~f:(fun _ scope -> Map.mem scope var)
  in
  match depth with
  | Some (depth, _) ->
      {
        resolver with
        resolved_locals = Map.set resolver.resolved_locals ~key:expr ~data:depth;
      }
  | None -> resolver

let rec resolve_statement statement resolver =
  match statement with
  | Statement.Block statements ->
      begin_scope resolver |> resolve statements |> end_scope
  | Var (name, init_expr) ->
      Option.fold ~init:(declare name resolver)
        ~f:(fun res expr -> resolve_expr expr res)
        init_expr
      |> define name
  | Function fn ->
      declare fn.name resolver |> define fn.name |> resolve_function fn
  | Expression expr -> resolve_expr expr resolver
  | If (cond, then_branch, else_branch) ->
      let resolver =
        resolve_expr cond resolver |> resolve_statement then_branch
      in
      Option.fold ~init:resolver
        ~f:(fun resolver stmt -> resolve_statement stmt resolver)
        else_branch
  | Print expr -> resolve_expr expr resolver
  | Return (_, expr) ->
      Option.fold ~init:resolver ~f:(fun res expr -> resolve_expr expr res) expr
  | While (cond, body) -> resolve_expr cond resolver |> resolve_statement body

and resolve_function fn resolver =
  let resolver = begin_scope resolver in
  Stdlib.List.fold_left
    (fun resolver param -> declare param resolver |> define param)
    resolver fn.params
  |> resolve fn.body |> end_scope

and resolve_expr expr resolver =
  match expr with
  | Expression.Variable name ->
      resolve_local expr (Token.get_identifier_name name) resolver
  | Assign (name, value) ->
      resolve_expr value resolver
      |> resolve_local expr (Token.get_identifier_name name)
  | Binary (left, _, right) -> resolve_expr left resolver |> resolve_expr right
  | Call (callee, _, args) ->
      let resolver = resolve_expr callee resolver in
      Stdlib.List.fold_left
        (fun resolver arg -> resolve_expr arg resolver)
        resolver args
  | Grouping expr -> resolve_expr expr resolver
  | Literal _ -> resolver
  | Logical (left, _, right) -> resolve_expr left resolver |> resolve_expr right
  | Unary (_, expr) -> resolve_expr expr resolver

and resolve statements resolver =
  Stdlib.List.fold_left
    (fun resolver stmt -> resolve_statement stmt resolver)
    resolver statements

let get_resolved_var_depth resolver expr =
  Map.find resolver.resolved_locals expr

let resolved_locals_to_string resolver =
  Map.fold resolver.resolved_locals ~init:"" ~f:(fun ~key ~data acc ->
      Printf.sprintf "%s\n%s -> %d" acc
        (Sexp.to_string (Expression.sexp_of_t key))
        data)
