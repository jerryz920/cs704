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

open Parsing;;
# 2 "LambdaParse.mly"
  open Lambda
# 34 "LambdaParse.ml"
let yytransl_const = [|
  257 (* LParen *);
  258 (* RParen *);
  259 (* LSquare *);
  260 (* RSquare *);
  261 (* LBrace *);
  262 (* RBrace *);
  263 (* Dot *);
  264 (* Arrow *);
  265 (* Tick *);
  266 (* Equals *);
  267 (* Eof *);
  268 (* Lambda *);
  269 (* If *);
  270 (* Then *);
  271 (* Else *);
  272 (* Let *);
  273 (* Letrec *);
  274 (* In *);
  275 (* Quote *);
  276 (* True *);
  277 (* False *);
  278 (* Nil *);
  279 (* Int *);
  280 (* Bool *);
  281 (* Sym *);
    0|]

let yytransl_block = [|
  282 (* ID *);
  283 (* Num *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\005\000\005\000\005\000\005\000\005\000\
\006\000\002\000\002\000\002\000\002\000\002\000\002\000\008\000\
\008\000\008\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\004\000\004\000\009\000\009\000\003\000\
\003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
\000\000"

let yylen = "\002\000\
\001\000\001\000\003\000\006\000\005\000\002\000\006\000\006\000\
\003\000\001\000\001\000\001\000\001\000\004\000\002\000\000\000\
\002\000\003\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\004\000\001\000\000\000\003\000\005\000\
\003\000\002\000\001\000\001\000\001\000\002\000\002\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\011\000\
\012\000\013\000\002\000\010\000\046\000\001\000\000\000\047\000\
\000\000\000\000\000\000\043\000\044\000\045\000\048\000\000\000\
\037\000\049\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\020\000\021\000\022\000\023\000\024\000\025\000\
\026\000\027\000\031\000\032\000\033\000\028\000\029\000\030\000\
\019\000\034\000\015\000\000\000\000\000\042\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\006\000\003\000\000\000\
\000\000\000\000\041\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\014\000\000\000\017\000\035\000\000\000\039\000\
\036\000\000\000\000\000\000\000\000\000\000\000\018\000\040\000\
\005\000\000\000\009\000\000\000\000\000\004\000\007\000\008\000"

let yydgoto = "\005\000\
\013\000\014\000\023\000\026\000\033\000\073\000\064\000\065\000\
\056\000"

let yysindex = "\003\000\
\047\255\112\255\140\255\135\255\000\000\103\255\076\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\239\254\000\000\
\140\255\140\255\238\254\000\000\000\000\000\000\000\000\000\255\
\000\000\000\000\009\255\047\255\010\255\011\255\076\255\047\255\
\014\255\076\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\005\255\015\255\000\000\248\254\018\255\
\250\254\007\255\255\254\255\254\042\255\000\000\000\000\016\255\
\043\255\140\255\000\000\000\255\140\255\044\255\047\255\037\255\
\048\255\049\255\000\000\076\255\000\000\000\000\050\255\000\000\
\000\000\047\255\034\255\047\255\035\255\036\255\000\000\000\000\
\000\000\047\255\000\000\047\255\047\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\051\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\053\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\053\255\
\000\000\000\000\000\000\051\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\250\255\056\000\253\255\000\000\000\000\255\255\252\255\254\255\
\248\255"

let yytablesize = 165
let yytable = "\032\000\
\025\000\031\000\051\000\001\000\002\000\003\000\004\000\054\000\
\055\000\057\000\059\000\060\000\066\000\052\000\053\000\063\000\
\034\000\068\000\067\000\070\000\071\000\058\000\076\000\069\000\
\072\000\062\000\061\000\035\000\036\000\037\000\038\000\039\000\
\040\000\041\000\042\000\043\000\044\000\045\000\046\000\047\000\
\048\000\049\000\050\000\075\000\078\000\082\000\084\000\006\000\
\090\000\085\000\086\000\088\000\092\000\093\000\016\000\007\000\
\038\000\016\000\074\000\080\000\000\000\077\000\079\000\000\000\
\083\000\081\000\008\000\009\000\010\000\000\000\000\000\087\000\
\011\000\012\000\000\000\089\000\034\000\091\000\000\000\000\000\
\000\000\000\000\000\000\094\000\000\000\095\000\096\000\035\000\
\036\000\037\000\038\000\039\000\040\000\041\000\042\000\043\000\
\044\000\045\000\046\000\047\000\048\000\049\000\050\000\006\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
\015\000\000\000\027\000\028\000\000\000\000\000\029\000\030\000\
\007\000\031\000\008\000\009\000\010\000\000\000\000\000\000\000\
\011\000\012\000\000\000\008\000\009\000\010\000\000\000\017\000\
\000\000\018\000\012\000\024\000\017\000\000\000\018\000\019\000\
\000\000\000\000\000\000\000\000\019\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\020\000\021\000\022\000\
\000\000\000\000\020\000\021\000\022\000"

let yycheck = "\006\000\
\004\000\019\001\007\000\001\000\002\000\003\000\004\000\026\001\
\009\001\001\001\001\001\001\001\008\001\017\000\018\000\002\001\
\001\001\026\001\004\001\026\001\014\001\028\000\007\001\006\001\
\026\001\032\000\031\000\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\002\001\002\001\002\001\010\001\001\001\
\015\001\002\001\002\001\002\001\018\001\018\001\002\001\009\001\
\006\001\002\000\060\000\068\000\255\255\064\000\066\000\255\255\
\071\000\069\000\020\001\021\001\022\001\255\255\255\255\076\000\
\026\001\027\001\255\255\082\000\001\001\084\000\255\255\255\255\
\255\255\255\255\255\255\090\000\255\255\092\000\093\000\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\001\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\009\001\
\001\001\255\255\012\001\013\001\255\255\255\255\016\001\017\001\
\009\001\019\001\020\001\021\001\022\001\255\255\255\255\255\255\
\026\001\027\001\255\255\020\001\021\001\022\001\255\255\001\001\
\255\255\003\001\027\001\005\001\001\001\255\255\003\001\009\001\
\255\255\255\255\255\255\255\255\009\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\023\001\024\001\025\001\
\255\255\255\255\023\001\024\001\025\001"

let yynames_const = "\
  LParen\000\
  RParen\000\
  LSquare\000\
  RSquare\000\
  LBrace\000\
  RBrace\000\
  Dot\000\
  Arrow\000\
  Tick\000\
  Equals\000\
  Eof\000\
  Lambda\000\
  If\000\
  Then\000\
  Else\000\
  Let\000\
  Letrec\000\
  In\000\
  Quote\000\
  True\000\
  False\000\
  Nil\000\
  Int\000\
  Bool\000\
  Sym\000\
  "

let yynames_block = "\
  ID\000\
  Num\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Lambda.value) in
    Obj.repr(
# 19 "LambdaParse.mly"
                               (Val _1)
# 223 "LambdaParse.ml"
               : Lambda.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 20 "LambdaParse.mly"
                               (Var _1)
# 230 "LambdaParse.ml"
               : Lambda.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_in_parens) in
    Obj.repr(
# 21 "LambdaParse.mly"
                               (_2)
# 237 "LambdaParse.ml"
               : Lambda.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Lambda.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Lambda.expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.expr) in
    Obj.repr(
# 25 "LambdaParse.mly"
                                        (If (_2, _4, _6))
# 246 "LambdaParse.ml"
               : 'expr_in_parens))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Lambda.expr) in
    Obj.repr(
# 26 "LambdaParse.mly"
                                        (Lambda (_3, _5))
# 254 "LambdaParse.ml"
               : 'expr_in_parens))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Lambda.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Lambda.expr) in
    Obj.repr(
# 27 "LambdaParse.mly"
                                        (Apply (_1, _2) )
# 262 "LambdaParse.ml"
               : 'expr_in_parens))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'def) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.expr) in
    Obj.repr(
# 28 "LambdaParse.mly"
                                        (Let (_3, _6))
# 270 "LambdaParse.ml"
               : 'expr_in_parens))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'def) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lambda.expr) in
    Obj.repr(
# 29 "LambdaParse.mly"
                                        (Letrec (_3, _6))
# 278 "LambdaParse.ml"
               : 'expr_in_parens))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lambda.expr) in
    Obj.repr(
# 32 "LambdaParse.mly"
                     (_1, _3)
# 286 "LambdaParse.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 35 "LambdaParse.mly"
         (Number _1)
# 293 "LambdaParse.ml"
               : Lambda.value))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "LambdaParse.mly"
         (Boolean true)
# 299 "LambdaParse.ml"
               : Lambda.value))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "LambdaParse.mly"
         (Boolean false)
# 305 "LambdaParse.ml"
               : Lambda.value))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "LambdaParse.mly"
         (Nil)
# 311 "LambdaParse.ml"
               : Lambda.value))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'quotation) in
    Obj.repr(
# 39 "LambdaParse.mly"
                                (_3)
# 318 "LambdaParse.ml"
               : Lambda.value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'quotation) in
    Obj.repr(
# 40 "LambdaParse.mly"
                 (_2)
# 325 "LambdaParse.ml"
               : Lambda.value))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "LambdaParse.mly"
  (Nil)
# 331 "LambdaParse.ml"
               : 'quotation_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'quotation) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'quotation_list) in
    Obj.repr(
# 45 "LambdaParse.mly"
                           (Pair(_1, _2))
# 339 "LambdaParse.ml"
               : 'quotation_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'quotation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'quotation) in
    Obj.repr(
# 46 "LambdaParse.mly"
                          (Pair(_1, _3))
# 347 "LambdaParse.ml"
               : 'quotation_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 49 "LambdaParse.mly"
      (Symbol _1)
# 354 "LambdaParse.ml"
               : 'quotation))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "LambdaParse.mly"
         (Symbol "lambda")
# 360 "LambdaParse.ml"
               : 'quotation))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "LambdaParse.mly"
     (Symbol "if")
# 366 "LambdaParse.ml"
               : 'quotation))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "LambdaParse.mly"
       (Symbol "then")
# 372 "LambdaParse.ml"
               : 'quotation))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "LambdaParse.mly"
       (Symbol "else")
# 378 "LambdaParse.ml"
               : 'quotation))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "LambdaParse.mly"
      (Symbol "let")
# 384 "LambdaParse.ml"
               : 'quotation))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "LambdaParse.mly"
         (Symbol "letrec")
# 390 "LambdaParse.ml"
               : 'quotation))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "LambdaParse.mly"
     (Symbol "in")
# 396 "LambdaParse.ml"
               : 'quotation))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "LambdaParse.mly"
        (Symbol "quote")
# 402 "LambdaParse.ml"
               : 'quotation))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "LambdaParse.mly"
      (Symbol "int")
# 408 "LambdaParse.ml"
               : 'quotation))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "LambdaParse.mly"
       (Symbol "bool")
# 414 "LambdaParse.ml"
               : 'quotation))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "LambdaParse.mly"
      (Symbol "sym")
# 420 "LambdaParse.ml"
               : 'quotation))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "LambdaParse.mly"
       (Boolean true)
# 426 "LambdaParse.ml"
               : 'quotation))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "LambdaParse.mly"
        (Boolean false)
# 432 "LambdaParse.ml"
               : 'quotation))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "LambdaParse.mly"
      (Nil)
# 438 "LambdaParse.ml"
               : 'quotation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 64 "LambdaParse.mly"
      (Number _1)
# 445 "LambdaParse.ml"
               : 'quotation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'quotation_list) in
    Obj.repr(
# 65 "LambdaParse.mly"
                               (_2)
# 452 "LambdaParse.ml"
               : 'quotation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'generic_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Lambda.typ) in
    Obj.repr(
# 68 "LambdaParse.mly"
                                       (_2, _4)
# 460 "LambdaParse.ml"
               : Lambda.gentyp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Lambda.typ) in
    Obj.repr(
# 68 "LambdaParse.mly"
                                                      ([], _1)
# 467 "LambdaParse.ml"
               : Lambda.gentyp))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "LambdaParse.mly"
              ([])
# 473 "LambdaParse.ml"
               : 'generic_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'generic_list) in
    Obj.repr(
# 69 "LambdaParse.mly"
                                          (_2::_3)
# 481 "LambdaParse.ml"
               : 'generic_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Lambda.typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Lambda.typ) in
    Obj.repr(
# 71 "LambdaParse.mly"
                              (Fun(_2,_4))
# 489 "LambdaParse.ml"
               : Lambda.typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lambda.typ) in
    Obj.repr(
# 72 "LambdaParse.mly"
                      (List _2)
# 496 "LambdaParse.ml"
               : Lambda.typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 73 "LambdaParse.mly"
          (Tvar _2)
# 503 "LambdaParse.ml"
               : Lambda.typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "LambdaParse.mly"
      (Int)
# 509 "LambdaParse.ml"
               : Lambda.typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "LambdaParse.mly"
       (Bool)
# 515 "LambdaParse.ml"
               : Lambda.typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "LambdaParse.mly"
      (Sym)
# 521 "LambdaParse.ml"
               : Lambda.typ))
(* Entry expr *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry value *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry typ *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry gentyp *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let expr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lambda.expr)
let value (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Lambda.value)
let typ (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : Lambda.typ)
let gentyp (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 4 lexfun lexbuf : Lambda.gentyp)
