open Base

type t = { values : Value.t option Map.M(String).t; enclosing : t option }
(* Runtime representation of a lexical environment. Holds mapping
   from variable identifier lexemes to evaluated Value results *)

let init () = { values = Map.empty (module String); enclosing = None }
let with_enclosing enclosing = { (init ()) with enclosing = Some enclosing }

let rec get env var =
  match Map.find env.values var with
  | None ->
      if Option.is_some env.enclosing then
        get (Option.value_exn env.enclosing) var
      else Error ("Undefined variable " ^ var)
  | Some v -> Ok v

let define env name value =
  { env with values = Map.set env.values ~key:name ~data:value }

let rec assign env name value =
  match Map.find env.values name with
  | None ->
      if Option.is_some env.enclosing then
        assign (Option.value_exn env.enclosing) name value
      else Error ("Undefined variable " ^ name)
  | Some _ -> Ok (define env name value)

let print env =
  Map.iteri env.values ~f:(fun ~key:name ~data:value ->
      Stdlib.Printf.printf "%s = %s\n%!" name (Value.to_string value))
