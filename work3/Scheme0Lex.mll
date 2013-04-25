{
  open Scheme0Parse
}

let id_body_char = ['a'-'z' 'A'-'Z' '0'-'9' '!' '?' '-' '_' '\'' '[' ']']
rule tokenize =
parse [' ' '\t' '\n' '\r']                   {tokenize lexbuf}
  | '('                                      {LParen}
  | ')'                                      {RParen}
  | "->"                                     {Arrow}
  | ':'                                      {Colon}
  | '.'                                      {Dot}
  | ['+' '-' '*' '=' '<' '>'] as s           {ID (String.make 1 s)}
  | '-'?['0'-'9']+ as i                      {Num (int_of_string i)}
  | ['a'-'z''A'-'Z'] id_body_char* as s      {ID s}
  | eof                                      {Eof}
  
