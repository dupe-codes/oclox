(* TODO: derive show to get to_string for free *)
type token_type =
  (* Single-character tokens *)
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  (* One or two character tokens *)
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  (* Literals *)
  | IDENTIFIER of string
  | STRING of string
  | NUMBER of float
  (* Keywords *)
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | COMMENT
  | WHITESPACE
  | NEWLINE
  | EOF

type t = { token_type : token_type; line : int }
(* NOTE:
    - All tokens associated with: lexeme + line number
    - Some tokens also have literal
    - Literal can be Double or String
*)

let init token_type line = { token_type; line }

let get_identifier_name = function
  | { token_type = IDENTIFIER s; _ } -> s
  | _ -> failwith "Not an identifier"

let type_to_string t_type =
  match t_type with
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | COMMA -> "COMMA"
  | DOT -> "DOT"
  | MINUS -> "MINUS"
  | PLUS -> "PLUS"
  | SEMICOLON -> "SEMICOLON"
  | SLASH -> "SLASH"
  | STAR -> "STAR"
  | BANG -> "BANG"
  | BANG_EQUAL -> "BANG_EQUAL"
  | EQUAL -> "EQUAL"
  | EQUAL_EQUAL -> "EQUAL_EQUAL"
  | GREATER -> "GREATER"
  | GREATER_EQUAL -> "GREATER_EQUAL"
  | LESS -> "LESS"
  | LESS_EQUAL -> "LESS_EQUAL"
  | IDENTIFIER _ -> "IDENTIFIER"
  | STRING _ -> "STRING"
  | NUMBER _ -> "NUMBER"
  | AND -> "AND"
  | CLASS -> "CLASS"
  | ELSE -> "ELSE"
  | FALSE -> "FALSE"
  | FUN -> "FUN"
  | FOR -> "FOR"
  | IF -> "IF"
  | NIL -> "NIL"
  | OR -> "OR"
  | PRINT -> "PRINT"
  | RETURN -> "RETURN"
  | SUPER -> "SUPER"
  | THIS -> "THIS"
  | TRUE -> "TRUE"
  | VAR -> "VAR"
  | WHILE -> "WHILE"
  | COMMENT -> "COMMENT"
  | WHITESPACE -> "WHITESPACE"
  | NEWLINE -> "NEWLINE"
  | EOF -> "EOF"

let get_lexeme token =
  match token.token_type with
  | LEFT_PAREN -> "("
  | RIGHT_PAREN -> ")"
  | LEFT_BRACE -> "{"
  | RIGHT_BRACE -> "}"
  | COMMA -> ","
  | DOT -> "."
  | MINUS -> "-"
  | PLUS -> "+"
  | SEMICOLON -> ";"
  | SLASH -> "/"
  | STAR -> "*"
  | BANG -> "!"
  | BANG_EQUAL -> "!="
  | EQUAL -> "="
  | EQUAL_EQUAL -> "=="
  | GREATER -> ">"
  | GREATER_EQUAL -> ">="
  | LESS -> "<"
  | LESS_EQUAL -> "<="
  | IDENTIFIER s | STRING s -> s
  | NUMBER f -> string_of_float f
  | AND -> "and"
  | CLASS -> "class"
  | ELSE -> "else"
  | FALSE -> "false"
  | FUN -> "fun"
  | FOR -> "for"
  | IF -> "if"
  | NIL -> "nil"
  | OR -> "or"
  | PRINT -> "print"
  | RETURN -> "return"
  | SUPER -> "super"
  | THIS -> "this"
  | TRUE -> "true"
  | VAR -> "var"
  | WHILE -> "while"
  | COMMENT -> "COMMENT"
  | WHITESPACE -> "WHITESPACE"
  | NEWLINE -> "NEWLINE"
  | EOF -> "EOF"

let to_string token =
  let type_str = type_to_string token.token_type in
  let lexeme = get_lexeme token in
  match token.token_type with
  | LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE | COMMA | DOT | MINUS
  | PLUS | SEMICOLON | SLASH | STAR | BANG | BANG_EQUAL | EQUAL | EQUAL_EQUAL
  | GREATER | GREATER_EQUAL | LESS | LESS_EQUAL | IDENTIFIER _ | STRING _
  | NUMBER _ ->
      type_str ^ " " ^ lexeme
  | _ -> String.uppercase_ascii lexeme

let get_identifier_type = function
  | "and" -> AND
  | "class" -> CLASS
  | "else" -> ELSE
  | "false" -> FALSE
  | "for" -> FOR
  | "fun" -> FUN
  | "if" -> IF
  | "nil" -> NIL
  | "or" -> OR
  | "print" -> PRINT
  | "return" -> RETURN
  | "super" -> SUPER
  | "this" -> THIS
  | "true" -> TRUE
  | "var" -> VAR
  | "while" -> WHILE
  | s -> IDENTIFIER s
