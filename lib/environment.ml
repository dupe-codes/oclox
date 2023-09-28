open Base

type t = {
  values : Value.t option Map.M(String).t;
  functions : Statement.fn_declaration Map.M(String).t;
  enclosing : t option;
}
(* Runtime representation of a lexical environment. Holds mapping
   from variable identifier lexemes to evaluated Value results, and function definition
   lexemes to function declarations *)

let init () =
  {
    values = Map.empty (module String);
    functions = Map.empty (module String);
    enclosing = None;
  }

let with_enclosing enclosing = { (init ()) with enclosing = Some enclosing }
let get_enclosing env = env.enclosing

let rec get env var =
  match Map.find env.values var with
  | None ->
      if Option.is_some env.enclosing then
        get (Option.value_exn env.enclosing) var
      else Error ("Undefined variable " ^ var)
  | Some v -> Ok v

let rec get_fn env fn_name =
  match Map.find env.functions fn_name with
  | None ->
      if Option.is_some env.enclosing then
        get_fn (Option.value_exn env.enclosing) fn_name
      else failwith "No such function."
  | Some v -> v

let define env name value =
  { env with values = Map.set env.values ~key:name ~data:value }

let define_fn env (fn_declaration : Statement.fn_declaration) =
  match fn_declaration.name.token_type with
  | Token.IDENTIFIER name ->
      {
        env with
        values =
          Map.set env.values ~key:name
            ~data:
              (Some
                 (Value.Function
                    { name; arity = List.length fn_declaration.params }));
        functions = Map.set env.functions ~key:name ~data:fn_declaration;
      }
  | _ -> failwith "Function name should be an identifier"

let rec assign env name value =
  match Map.find env.values name with
  | None ->
      if Option.is_some env.enclosing then
        assign (Option.value_exn env.enclosing) name value
        |> Result.map ~f:(fun enclosing ->
               { env with enclosing = Some enclosing })
      else Error ("Undefined variable " ^ name)
  | Some _ -> Ok (define env name value)

let print env =
  Map.iteri env.values ~f:(fun ~key:name ~data:value ->
      Stdlib.Printf.printf "%s = %s\n%!" name (Value.to_string value))

let global () =
  let env = init () in
  define env "clock"
    (Some
       (Value.Native
          ( { name = "clock"; arity = 0 },
            fun _ -> Some (Value.Float (Unix.time ())) )))
