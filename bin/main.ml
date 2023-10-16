(* Main REPL and source file executor for the Lox Programming Language *)

let run source env =
  let open Oclox in
  Scanner.init source |> Scanner.scan_tokens |> fun token_result ->
  Result.bind token_result (fun tokens ->
      let parser = Parser.init tokens in
      match Parser.parse parser with
      | Some statements ->
          let resolver = Resolver.init () |> Resolver.resolve statements in
          let _ =
            Printf.printf "Resolved variables:\n%s\n\n%!"
              (Resolver.resolved_locals_to_string resolver)
          in
          let _ =
            Printf.printf "AST:\n%s\n\n%!"
              (Ast_printer.print_statements statements)
          in
          Interpreter.interpret statements env resolver
      | None -> Lox_error.init 0 "Parsing failure")

let rec run_prompt env =
  let _ = Printf.printf "> %!" in
  let line = Stdio.In_channel.input_line Stdio.stdin in
  match line with
  | None -> ()
  | Some line ->
      let _ = run line env in
      run_prompt env

let run_file file_name =
  let open Oclox in
  let file_contents = Stdio.In_channel.read_all file_name in
  Result.iter_error
    (fun error_type ->
      match error_type with
      | Lox_error.Syntax_error -> exit 65
      | Lox_error.Runtime_error -> exit 70)
    (run file_contents (Environment.global ()))

let _ =
  let open Oclox in
  let subs =
    [
      ("a", Types.TypeConstructor Types.Int);
      ("b", Types.TypeConstructor Types.Bool);
      ( "c",
        Types.TypeConstructor
          (Types.Arrow
             [
               Types.TypeConstructor Types.Int; Types.TypeConstructor Types.Int;
             ]) );
    ]
  in
  let ctx =
    [
      ("a", Types.MonoType (Types.TypeVar "a"));
      ( "i",
        Types.UniversallyQuantified
          ( "f",
            Types.MonoType
              (Types.TypeConstructor
                 (Types.Arrow
                    [ Types.TypeVar "b"; Types.TypeConstructor Types.Bool ])) )
      );
    ]
  in
  let context = Type_inference.init_context ctx in
  let s1 = Type_inference.init_substitution subs in
  let transformed_ctx =
    Type_inference.apply s1 (Type_inference.Context context)
  in
  Format.printf "@[<v 2>Transformed context:@,%a@,@]"
    Type_inference.pp_substitution_target transformed_ctx

(*let _ =*)
(*let args_length = Array.length Sys.argv in*)
(*let errored_invocation = if args_length > 2 then true else false in*)
(*if errored_invocation then*)
(*let _ = Printf.printf "Usage: oclox [script]" in*)
(*exit 64*)
(*else if Array.length Sys.argv = 2 then run_file Sys.argv.(1)*)
(*else run_prompt (Oclox.Environment.global ())*)
