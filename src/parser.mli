exception Error

type token = 
  | WITH
  | TYPE
  | STAR
  | SEMISEMI
  | RPAR
  | REC
  | RBRACKET
  | RANGLE
  | QUOTE
  | QUESTIONMARK
  | OF
  | MATCH
  | LPAR
  | LET
  | LBRACKET
  | LANGLE
  | LAM
  | IN
  | IDENTIFIER of (string)
  | FUN
  | EOF
  | END
  | DOUBLEARROW
  | DOT
  | DEFEQ
  | CONSTRUCTOR of (string)
  | COMMA
  | COLON
  | BEGIN
  | BAR
  | ARROW
  | AND


val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)