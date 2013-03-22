type token =
  | LParen
  | RParen
  | LSquare
  | RSquare
  | LBrace
  | RBrace
  | Dot
  | Arrow
  | Tick
  | Equals
  | Eof
  | Lambda
  | If
  | Then
  | Else
  | Let
  | Letrec
  | In
  | Quote
  | True
  | False
  | Nil
  | Int
  | Bool
  | Sym
  | ID of (string)
  | Num of (int)

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.expr
val value :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.value
val typ :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.typ
val gentyp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.gentyp
