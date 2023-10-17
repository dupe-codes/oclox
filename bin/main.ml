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
      ("a", Types.TypeFunctionApplication Types.Int);
      ("b", Types.TypeFunctionApplication Types.Bool);
      ( "c",
        Types.TypeFunctionApplication
          (Types.Arrow
             [
               Types.TypeFunctionApplication Types.Int;
               Types.TypeFunctionApplication Types.Int;
             ]) );
      ("d", Types.TypeFunctionApplication Types.Unit);
    ]
  in
  let ctx =
    [
      ("a", Types.MonoType (Types.TypeVar "a"));
      ( "f",
        Types.Quantified
          ( "i",
            Types.MonoType
              (Types.TypeFunctionApplication
                 (Types.Arrow [ Types.TypeVar "i"; Types.TypeVar "b" ])) ) );
      ( "g",
        Types.MonoType
          (Types.TypeFunctionApplication
             (Types.Arrow [ Types.TypeVar "d"; Types.TypeVar "d" ])) );
    ]
  in
  let context = Type_inference.init_context ctx in
  let s1 = Type_inference.init_substitution subs in
  let _ =
    Format.printf "@[<v 2>Substitution:@,%a@,@]"
      Types_printer.print_substitution s1
  in
  let _ =
    Format.printf "\n\n@[<v 2>Context:@,%a@,@]" Types_printer.print_context
      context
  in
  let transformed_ctx =
    Type_inference.apply s1 (Type_inference.Context context)
  in
  let _ =
    Format.printf "\n\n@[<v 2>Transformed context:@,%a@,@]"
      Types_printer.print_substitution_target transformed_ctx
  in
  let s2 =
    Type_inference.init_substitution
      [
        ( "z",
          Types.TypeFunctionApplication
            (Types.Arrow [ Types.TypeVar "zz"; Types.TypeVar "a" ]) );
        ("x", Types.TypeVar "f");
        ( "y",
          Types.TypeFunctionApplication
            (Types.Arrow
               [
                 Types.TypeVar "c";
                 Types.TypeFunctionApplication
                   (Types.Arrow [ Types.TypeVar "b"; Types.TypeVar "u" ]);
               ]) );
      ]
  in
  let _ =
    Format.printf "\n\n@[<v 2>Substitution 2:@,%a@,@]"
      Types_printer.print_substitution s2
  in
  let _ =
    Format.printf "\n\n@[<v 2>Combined substitution:@,%a@,@]"
      Types_printer.print_substitution_target
      (Type_inference.apply s1 (Type_inference.Substitution s2))
  in
  let poly =
    Types.MonoType
      (Types.TypeFunctionApplication
         (Types.Arrow [ Types.TypeVar "a"; Types.TypeVar "b" ]))
  in
  let _ =
    Format.printf "\n\n@[<v 2>Transformed polytype:@,%a@,@]"
      Types_printer.print_substitution_target
      (Type_inference.apply s1 (Type_inference.PolyType poly))
  in
  let mono = Types.TypeVar "a" in
  Format.printf "\n\n@[<v 2>Transformed monotype:@,%a@,@]"
    Types_printer.print_substitution_target
    (Type_inference.apply s1 (Type_inference.MonoType mono))

(*let _ =*)
(*let args_length = Array.length Sys.argv in*)
(*let errored_invocation = if args_length > 2 then true else false in*)
(*if errored_invocation then*)
(*let _ = Printf.printf "Usage: oclox [script]" in*)
(*exit 64*)
(*else if Array.length Sys.argv = 2 then run_file Sys.argv.(1)*)
(*else run_prompt (Oclox.Environment.global ())*)
