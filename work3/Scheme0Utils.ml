(* Regular Scheme0 AST *)
type value =
  | Number of int
  | Boolean of bool
  | Symbol of string
  | Pair of value * value
  | Nil

and expr =
  | Val of value
  | Var of string
  | Call of string * (expr list)
  | If of expr * expr * expr
  | Binop of binop * expr * expr  
  | Unop of unop * expr

and binop = Cons | Plus | Minus | Times | Div | Mod
                 | Equals | LessThan | GreaterThan
and unop = Car | Cdr | IsNil

(* Function definition *)
and definition = string list * expr (* list of formals * body *)
(* Program: Association list. Maps function names to definitions.
   The goal function is named "main". *)
and program = (string * definition) list

(****** Binding-time Types ******)

(* bind time: Static (S) or Dynamic (D). *)
type bind_time = S | D
(* bind time environment: bind times of parameters and the function's value. *)
type bind_time_env = (string * bind_time) list * bind_time
(* division: Association list.
             Maps function names to bind-time environments.
*)
type division = (string * bind_time_env) list

(****** Exceptions ******)
(* A SyntaxError indicates a syntax error in the input Scheme0 program. *)
exception SyntaxError of string
(* A UtilityError indicates an implementation error in Scheme0Utils. *)
exception UtilityError 
(* An EvalError indicates an evaluation error of a Scheme0 program. *)
exception EvalError of string

(****** Output functions ******)
(* These are the full, general formatters. 
   ppf is, in each function, a pretty-printing formatter of the Format module.
*)
let rec fmt_program ppf (prog:program) =
  Format.fprintf ppf "@[<hv 0>";
  List.iter (fmt_name_and_def ppf) prog;
  Format.fprintf ppf "@]";

and fmt_name_and_def ppf (name, (params, e):string * definition) =
  Format.fprintf ppf "@[<hov 2>(define@ @[<hov 2>(%s@ %a)@]@ %a)@]@."
    name
    fmt_params params
    fmt_expr e

and fmt_params ppf = function
  | [] -> ()
  | (p :: []) -> Format.fprintf ppf "%s@," p
  | (p :: rest) -> Format.fprintf ppf "%s@ %a" p fmt_params rest

and fmt_expr ppf (e:expr) =
  match e with
    | Val(v) ->
      fmt_value false ppf v
    | Var(s) ->
      Format.fprintf ppf "%s" s
    | Call(s, el) ->
      Format.fprintf ppf "@[<hov 2>(call@ %s@ %a)@]" s fmt_expr_list el
    | If(cond, e1, e2) ->
      Format.fprintf ppf "@[<hov 2>(if@ %a@ %a@ %a)@]"
        fmt_expr cond fmt_expr e1 fmt_expr e2
    | Binop(op, e1, e2) ->
      Format.fprintf ppf "@[<hov 2>(%s@ %a@ %a)@]"
        (string_of_binop op) fmt_expr e1 fmt_expr e2        
    | Unop(op, e) ->
      Format.fprintf ppf "@[<hov 2>(%s@ %a)@]"
        (string_of_unop op) fmt_expr e

and fmt_expr_list ppf (el:expr list) =
  match el with
    | [] -> ()
    | (e :: []) -> Format.fprintf ppf "%a@," fmt_expr e
    | (e :: rest) -> Format.fprintf ppf "%a@ %a" fmt_expr e fmt_expr_list rest

and fmt_value (inquote:bool) ppf = function
  | Number(i) -> Format.fprintf ppf "%i" i
  | Boolean(b) -> Format.fprintf ppf "%B" b
  | Symbol(s) -> if inquote
      then Format.fprintf ppf "%s" s
      else Format.fprintf ppf "(quote %s)" s
  | Nil -> Format.fprintf ppf "nil"
  (* This is slightly fancy to make lists look nice. *)
  | Pair(v, Pair(a,b)) ->
    let fprint = if inquote
      then Format.fprintf ppf "@[<hov 2>(%a@ %a)@]"
      else Format.fprintf ppf "@[<hov 2>(quote@ @[<hov 2>(%a@ %a)@])@]" in
    fprint (fmt_value true) v fmt_more_list_value (a,b)
  | Pair(v, Nil) ->
    if inquote
    then Format.fprintf ppf "@[<hov 2> (%a)@]" (fmt_value true) v
    else Format.fprintf ppf "@[<hov 2>(quote@ (%a))@]" (fmt_value true) v
  | Pair(a,b) ->
    let fprint = if inquote
      then Format.fprintf ppf "@[<hov 2>(%a@ .@ %a)@]"
      else Format.fprintf ppf "@[<hov 2>(quote(@ %a@ . %a))@]" in
    fprint (fmt_value inquote) a (fmt_value inquote) b
      
and fmt_more_list_value ppf = function
  | (v, Pair(a,b)) -> Format.fprintf ppf "%a@ %a"
    (fmt_value true) v fmt_more_list_value (a,b)
  | (v, Nil) -> Format.fprintf ppf "%a"
    (fmt_value true) v
  | (v1, v2) -> Format.fprintf ppf "%a@ .@ %a"
    (fmt_value true) v1 (fmt_value true) v2

and string_of_unop = function
  | Car -> "car"
  | Cdr -> "cdr"
  | IsNil -> "nil?"

and string_of_binop = function
  | Cons -> "cons"
  | Equals -> "="
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "div"
  | Mod -> "mod"
  | LessThan -> "<"
  | GreaterThan -> ">"

and string_of_bt = function | S -> "S" | D -> "D" 

and fmt_bt_env ppf ((params,ret_bt):bind_time_env) =
  let rec fmt_params ppf ps = match ps with
    | (p_name,p_bt) :: tail ->
        Format.fprintf ppf "%s:%s@ %a"
          p_name (string_of_bt p_bt)
          fmt_params tail
    | [] -> () in
  Format.fprintf ppf "%a->@ %s" fmt_params params (string_of_bt ret_bt)

and fmt_division ppf (div:division) =
  match div with
    | (name, bte) :: tail -> Format.fprintf ppf "@[<hov 2>(%s@ %a)@]@.%a"
      name fmt_bt_env bte fmt_division tail
    | [] -> ()

(* Output functions. *)
let print_expr e = fmt_expr Format.std_formatter e
let print_program p = fmt_program Format.std_formatter p
let print_value v = fmt_value false Format.std_formatter v
let print_division d = fmt_division Format.std_formatter d

let string_of_expr (e:expr) :string =
  fmt_expr Format.str_formatter e; Format.flush_str_formatter ()
let string_of_program p =
  fmt_program Format.str_formatter p; Format.flush_str_formatter ()
let string_of_value v =
  fmt_value false Format.str_formatter v; Format.flush_str_formatter ()
let string_of_division d =
  fmt_division Format.str_formatter d; Format.flush_str_formatter ()

(* Input. These are a little bit horrible. Sorry. -me *)

module PT = Scheme0ParseTree

exception SyntaxError of string
exception UtilityError
exception EvalError of string

(* These are some error messages. They're really ugly inline. *)
let err_bad_id_as_param = SyntaxError
  "The parameters in a function definition must be valid identifiers." ;;
let err_bad_def = SyntaxError
  "Bad definition: each function definition should look like this:

  (define (<function name> <parameter name>*) <expression>)

where <expression> is either a number, an identifier, or an s-expression." ;; 
let err_bad_call_arg = SyntaxError
"The 'call' operator's first argument must be the function it's calling." ;;
let err_sexpr_first = SyntaxError
"The first element of an unquoted s-expression must be 'call', 'if', 'quote',
or one of the operators." ;;
let err_a_sexpr_first = SyntaxError
"The first element of an unquoted s-expression must be 
'call', 'if', 'quote', or one of the operators." ;;

let err_expected_id = SyntaxError "Expected an identifier as a parameter";;

let err_if_takes_three = SyntaxError 
  "The 'if' operator takes exactly three arguments." ;;

let err_bad_bte = SyntaxError
  "Bad bind-time environment. A bind-time environment should look like this:

  (<function name> <param>:<bt> <param>:<bt> ... <param>:<bt> -> <bt>)

where each <bt> is one of 'S' or 'D'. There may be zero <params>, in which case
the bind-time environment looks like: (<function name> -> <bt>)." ;;

let err_bad_bt = SyntaxError "Bad bind-time symbol. Bind times are 'S' or 'D'."
let err_colon_expr = SyntaxError "Can't begin an expression with a colon: ':'."
let err_arrow_expr = SyntaxError "Can't begin an expression with an arrow: '->'."
type op_class = Unary | Binary | IfClass | CallClass | Nope
let class_of = function
  | "if" -> IfClass
  | "cons" | "+" | "-" | "*" | "div" | "mod" | "=" | "<" | ">" -> Binary
  | "car" | "cdr" | "nil?" | "quote" -> Unary
  | "call" -> CallClass
  | _ -> Nope

let string_of_symbol = function
  | PT.Symbol s -> s
  | _ -> raise err_expected_id ;;  

let bt_of_symbol = function
  | PT.Symbol "S" -> S
  | PT.Symbol "D" -> D
  | _ -> raise err_bad_bt ;;

let rec parse_atoms_as_prog (atoms:PT.program) :program =
  List.map parse_atom_as_named_def atoms

and parse_atom_as_named_def (pt_def :PT.atom) :string * definition =
  match pt_def with
    | PT.SExpr [PT.Symbol "define";
                PT.SExpr ((PT.Symbol func_name)::args);
                e] ->
      let parsed_params = List.map string_of_symbol args
      in func_name, (parsed_params, parse_atom_as_expr e)
    | _ -> raise err_bad_def

and parse_atom_as_expr = function
  | PT.Numeral i -> Val(Number i)
  | PT.Symbol "true" -> Val(Boolean true)
  | PT.Symbol "false" -> Val(Boolean false)
  | PT.Symbol "nil" -> Val(Nil)
  | PT.Symbol s -> Var s
  | PT.SExpr e -> parse_expr_as_expr e
  | PT.Improper _ -> raise (SyntaxError "Improper lists must be quoted.")
  | PT.Colon -> raise err_colon_expr
  | PT.Arrow -> raise err_arrow_expr

and parse_atom_as_quote (a:PT.atom) :value=
  let parse_and_cons atom right = Pair(parse_atom_as_quote atom, right) in
  match a with
    | PT.Numeral i -> Number i
    | PT.Symbol "true" -> Boolean true
    | PT.Symbol "false" -> Boolean false
    | PT.Symbol "nil" -> Nil
    | PT.Symbol s -> Symbol s
    | PT.SExpr e -> List.fold_right parse_and_cons e Nil
    | PT.Improper (e,dangle) ->
      List.fold_right parse_and_cons e (parse_atom_as_quote dangle)
    | PT.Colon -> raise err_colon_expr
    | PT.Arrow -> raise err_arrow_expr

and parse_expr_as_expr (pt_expr:PT.expr) :expr =
  match pt_expr with
    | [] -> Val Nil 
    | PT.Symbol(s)::args -> (
      match class_of s with 
        | Unary -> parse_unop s args
        | Binary -> parse_binop s args
        | IfClass -> (
          match args with 
            | [a;b;c] -> If(parse_atom_as_expr a,
                            parse_atom_as_expr b,
                            parse_atom_as_expr c)
            | _ -> raise err_if_takes_three
        )
        | CallClass -> (
          match args with
            | (PT.Symbol func_name) :: func_args ->
              Call(func_name, List.map parse_atom_as_expr func_args)
            | _ -> raise err_bad_call_arg
        )
        | Nope -> raise (SyntaxError (s^" isn't a valid operator."))
    )
    | _ :: _ -> raise err_sexpr_first


and parse_unop s (args:PT.expr) :expr =
  match args with
    | [a] -> ( match s with
        | "car" -> Unop(Car, parse_atom_as_expr a)
        | "cdr" -> Unop(Cdr, parse_atom_as_expr a)
        | "nil?" -> Unop(IsNil, parse_atom_as_expr a)
        | "quote" -> Val(parse_atom_as_quote a)
        | _ -> raise UtilityError
    )
    | _ -> raise (SyntaxError ("The "^s^" operator takes exactly one argument."))

and parse_binop s args =
  match args with
    | [p1; p2] ->
      ( let e1 = parse_atom_as_expr p1 in
        let e2 = parse_atom_as_expr p2 in
        match s with
          | "cons" -> Binop(Cons, e1, e2)
          | "+" -> Binop(Plus, e1, e2)
          | "-" -> Binop(Minus, e1, e2)
          | "*" -> Binop(Times, e1, e2)
          | "div" -> Binop(Div, e1, e2)
          | "mod" -> Binop(Mod, e1, e2)
          | "=" -> Binop(Equals, e1, e2)
          | "<" -> Binop(LessThan, e1, e2)
          | ">" -> Binop(GreaterThan, e1, e2)
          | _ -> raise UtilityError
      )
    | _ -> raise (SyntaxError("The "^s^" operator takes exactly two arguments."))

;;
        
let rec parse_atom_list_as_division (atoms:PT.atom list) :division = 
  List.map parse_atom_as_named_bte atoms

and parse_atom_as_named_bte (atom:PT.atom) :(string * bind_time_env) =
  match atom with
    | PT.SExpr ((PT.Symbol fname)::tail) -> fname, parse_bte tail
    | _ -> raise err_bad_bte

and parse_bte (atoms:PT.atom list) :bind_time_env =
  match atoms with 
    | (PT.Symbol pname) :: (PT.Colon) :: bt_sym :: tail ->
      let bt = bt_of_symbol bt_sym in 
      let tail_params, ret_bt = parse_bte tail in
      (pname,bt) :: tail_params, ret_bt
    | PT.Arrow :: bt_sym :: [] -> [], bt_of_symbol bt_sym
    | _ -> raise err_bad_bte
;;

let program_from_stdin () =
  let lexbuf = Lexing.from_channel stdin in
  let parsetree = Scheme0Parse.program Scheme0Lex.tokenize lexbuf in
  parse_atoms_as_prog parsetree
;;

let program_from_file (filename:string) = 
  let lexbuf = Lexing.from_channel (open_in filename) in
  let parsetree = Scheme0Parse.program Scheme0Lex.tokenize lexbuf in
  parse_atoms_as_prog parsetree
;;

let division_from_stdin () = 
  let lexbuf = Lexing.from_channel stdin in
  let parsetree = Scheme0Parse.program Scheme0Lex.tokenize lexbuf in
  parse_atom_list_as_division parsetree
;;

let division_from_file (filename:string) = 
  let lexbuf = Lexing.from_channel (open_in filename) in
  let parsetree = Scheme0Parse.program Scheme0Lex.tokenize lexbuf in
  parse_atom_list_as_division parsetree
;;

let value_from_string s = 
  let lexbuf = Lexing.from_string s in
  let parsetree = Scheme0Parse.atom Scheme0Lex.tokenize lexbuf in
  parse_atom_as_quote parsetree
;;

(****** residue_name: determines the name of a specialized function. ******)


let residue_name f_name static_vals =
  let val_rep v =
    Str.global_replace (Str.regexp " ") "_" 
      (Str.global_replace (Str.regexp "(") "[" 
         (Str.global_replace (Str.regexp ")") "]" 
            (string_of_value v) ))
  in
  List.fold_left (fun s v -> s ^ "-" ^ (val_rep v)) f_name static_vals
;;



