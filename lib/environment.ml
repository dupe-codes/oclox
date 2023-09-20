open Base

type t = { values : Value.t option Map.M(String).t }
(* Runtime representation of a lexical environment. Holds mapping
   from variable identifier lexemes to evaluated Value results *)

let init () = { values = Map.empty (module String) }

let get env var =
  match Map.find env.values var with
  | None -> Error ("Undefined variable " ^ var)
  | Some v -> Ok v

let define env name value =
  { values = Map.set env.values ~key:name ~data:value }

let print env =
  Map.iteri env.values ~f:(fun ~key:name ~data:value ->
      Stdlib.Printf.printf "%s = %s\n%!" name (Value.to_string value))
