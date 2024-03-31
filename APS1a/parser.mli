type token =
  | LBRA
  | RBRA
  | LPAR
  | RPAR
  | SEMCOL
  | COL
  | COMA
  | ARROW
  | STAR
  | CONST
  | FUN
  | REC
  | VAR
  | PROC
  | ECHO
  | SET
  | IF
  | WHILE
  | CALL
  | VOID
  | AND
  | OR
  | BOOL
  | INT
  | ADR
  | VARADR
  | NUM of (int)
  | IDENT of (string)

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.block
