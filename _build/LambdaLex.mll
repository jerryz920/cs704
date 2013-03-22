{
  open LambdaParse
}

let digit = ['0'-'9']
let id_first = ['a'-'z' 'A'-'Z']
let id_rest = ['a'-'z' 'A'-'Z' '0'-'9']
let whitespace = [' ' '\t' '\n' '\r']
let op = ['+' '-' '*' '<' '>']

  rule tokenize = parse
  whitespace {tokenize lexbuf}
    | '(' {LParen}
    | ')' {RParen}
    | '[' {LSquare}
    | ']' {RSquare}
    | '{' {LBrace}
    | '}' {RBrace}
    | '.' {Dot}
| "->" {Arrow}
| '\'' {Tick}
| "lambda" {Lambda}
| "if" {If}
| "then" {Then}
| "else" {Else}
| "let" {Let}
| "="  {Equals}
| "in" {In}
| "letrec" {Letrec}
| "quote" {Quote}
| "true" {True}
| "false" {False}
| "nil" {Nil}
| "int" {Int}
| "bool" {Bool}
| "sym" {Sym}
| '-'?digit+ as i {Num (int_of_string i)}
| id_first id_rest* as s  {ID s}
| op as s         {ID (String.make 1 s)}
| "=="            {ID "=="}
| eof {Eof}

