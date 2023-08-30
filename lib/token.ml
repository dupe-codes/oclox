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
  | EOF

type t = { token_type : token_type; line : int }
(* NOTE:
    - All tokens associated with: lexeme + line number
    - Some tokens also have literal
    - Literal can be Double or String
*)

let to_string token =
  match token.token_type with
  | LEFT_PAREN -> "LEFT_PAREN ("
  | RIGHT_PAREN -> "RIGHT_PAREN )"
  | LEFT_BRACE -> "LEFT_BRACE {"
  | RIGHT_BRACE -> "RIGHT_BRACE }"
  | COMMA -> "COMMA ,"
  | DOT -> "DOT ."
  | MINUS -> "MINUS -"
  | PLUS -> "PLUS +"
  | SEMICOLON -> "SEMICOLON ;"
  | SLASH -> "SLASH /"
  | STAR -> "STAR *"
  | BANG -> "BANG !"
  | BANG_EQUAL -> "BANG_EQUAL !="
  | EQUAL -> "EQUAL ="
  | EQUAL_EQUAL -> "EQUAL_EQUAL =="
  | GREATER -> "GREATER >"
  | GREATER_EQUAL -> "GREATER_EQUAL >="
  | LESS -> "LESS <"
  | LESS_EQUAL -> "LESS_EQUAL <="
  | IDENTIFIER s -> "IDENTIFIER " ^ s
  | STRING s -> "STRING " ^ s
  | NUMBER f -> "NUMBER " ^ string_of_float f
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
  | EOF -> "EOF"
