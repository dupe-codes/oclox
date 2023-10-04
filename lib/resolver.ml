open Base

module ExprKey = struct
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
type resolver = { scopes : scopes list; resolved_locals : locals }

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
let resolve_local expr name resolver = resolver

let rec resolve_statement statement resolver =
  match statement with
  | Statement.Block statements ->
      begin_scope resolver |> resolve statements |> end_scope
  | Var (name, init_expr) ->
      Option.fold ~init:(declare name resolver)
        ~f:(fun res expr -> resolve_expr expr res)
        init_expr
      |> define name
  | _ -> failwith "not implemented"

and resolve_expr expr resolver =
  match expr with
  | Expression.Variable name ->
      resolve_local expr (Token.get_identifier_name name) resolver
  | _ -> failwith "not implemented"

and resolve statements resolver =
  Stdlib.List.fold_left
    (fun resolver stmt -> resolve_statement stmt resolver)
    resolver statements
