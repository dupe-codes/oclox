open Base

let rec print_type_function fmt = function
  | Types.Int -> Stdlib.Format.fprintf fmt "Int"
  | Types.Bool -> Stdlib.Format.fprintf fmt "Bool"
  | Types.Unit -> Stdlib.Format.fprintf fmt "()"
  | Types.Arrow types ->
      let rec print_args = function
        | [] -> ()
        | [ last ] -> print_mono_type fmt last
        | arg :: rest ->
            print_mono_type fmt arg;
            Stdlib.Format.fprintf fmt " -> ";
            print_args rest
      in
      print_args types

and print_mono_type fmt = function
  | Types.TypeVar var -> Stdlib.Format.fprintf fmt "%s" var
  | Types.TypeFunctionApplication func -> print_type_function fmt func

and print_poly_type fmt = function
  | Types.MonoType mono -> print_mono_type fmt mono
  | Types.Quantified (type_var, poly) ->
      Stdlib.Format.fprintf fmt "∀ %s, " type_var;
      print_poly_type fmt poly

let print_context fmt (context : Type_inference.context) =
  let pp_item fmt (key, data) =
    Stdlib.Format.fprintf fmt "%s : " key;
    print_poly_type fmt data
  in
  Map.to_alist context
  |> List.iter ~f:(fun item ->
         pp_item fmt item;
         Stdlib.Format.fprintf fmt "; ")

let print_substitution fmt (substitution : Type_inference.substitution) =
  let pp_item fmt (key, data) =
    Stdlib.Format.fprintf fmt "%s => " key;
    print_mono_type fmt data
  in
  (* Convert the substitution map to a list and print each item *)
  Map.to_alist substitution
  |> List.iter ~f:(fun item ->
         pp_item fmt item;
         Stdlib.Format.fprintf fmt "; ")

let print_substitution_target fmt = function
  | Type_inference.Context context ->
      Stdlib.Format.fprintf fmt "Context(";
      print_context fmt context;
      Stdlib.Format.fprintf fmt ")"
  | Type_inference.Substitution substitution ->
      Stdlib.Format.fprintf fmt "Substitution(";
      print_substitution fmt substitution;
      Stdlib.Format.fprintf fmt ")"
  | Type_inference.PolyType poly_type ->
      Stdlib.Format.fprintf fmt "PolyType(";
      print_poly_type fmt poly_type;
      Stdlib.Format.fprintf fmt ")"
  | Type_inference.MonoType mono_type ->
      Stdlib.Format.fprintf fmt "MonoType(";
      print_mono_type fmt mono_type;
      Stdlib.Format.fprintf fmt ")"
