open Lambda ;;
open Format ;;

(* printers to general formatters *)
(* ^^ is concatenation for the mildy-magical format6 type. *)
let p fmt = "@[<hov 2>(" ^^ fmt ^^ ")@]"

let rec fmt_expr ppf (e:expr) = 
  match e with 
    | Val v ->
      fmt_value false ppf v
    | Var s ->
      pp_print_string ppf s
    | If(e1, e2, e3) ->
      fprintf ppf (p "if@ %a@;%a@;%a") fmt_expr e1 fmt_expr e2 fmt_expr e3
    | Lambda(param, body) ->
      fprintf ppf (p "lambda@ (%a)@;%a") pp_print_string param fmt_expr body
    | Apply(func, arg) ->
      fprintf ppf (p "%a@ %a") fmt_expr func fmt_expr arg
    | Let(def, body) ->
      fprintf ppf (p "let@ (%a)@;%a") fmt_def def fmt_expr body
    | Letrec(def, body) ->
      fprintf ppf (p "letrec@ (%a)@;%a") fmt_def def fmt_expr body
and fmt_str_list ppf (strs: string list) =
  pp_print_string ppf (String.concat " " strs) 
and fmt_expr_list ppf (es: expr list) = 
  match es with 
    | [] -> ()
    | e :: [] -> fmt_expr ppf e
    | e :: tail -> fprintf ppf "%a@ %a" fmt_expr e fmt_expr_list tail
and fmt_def ppf (def: (string * expr)) =
  match def with
    | (var, e) -> fprintf ppf (p "%s@;%a") var fmt_expr e

and fmt_value (inquote:bool) ppf = function
  | Number i -> fprintf ppf "%i" i
  | Boolean b -> fprintf ppf "%B" b
  | Symbol s -> if inquote then fprintf ppf "%s" s else fprintf ppf "'%s" s
  | Nil -> fprintf ppf "nil"
  (* This is fancy so that lists are readable. *)
  | Pair(v, rest) ->
    let fmt = if inquote then (p "%a%a") else ("'" ^^ (p "%a%a")) in
    fprintf ppf fmt (fmt_value true) v fmt_list_tail rest
and fmt_list_tail ppf = function
  | Nil -> ()
  | Number i -> fprintf ppf "@ .@ %i" i
  | Boolean b -> fprintf ppf "@ .@ %B" b
  | Symbol s -> fprintf ppf "@ .@ %s" s
  | Pair(v, rest) -> fprintf ppf "@ %a%a" (fmt_value true) v fmt_list_tail rest

let rec fmt_typ ppf in_t = 
  fprintf ppf "@[<hov 2>";
  ( match in_t with
    | Int -> pp_print_string ppf "int"
    | Bool -> pp_print_string ppf "bool"
    | Sym -> pp_print_string ppf "sym"
    | Tvar v -> fprintf ppf "'%s" v
    | List t -> fprintf ppf "[%a]" fmt_typ t
    | Fun(param, t) -> fprintf ppf (p "%a->@ %a") fmt_typ param fmt_typ t
  );
  fprintf ppf "@]"

let rec fmt_gentyp ppf ((typvars, t):gentyp) =
  fprintf ppf "{@[<hov 2>%a@]} %a" fmt_typvars typvars fmt_typ t
and fmt_typvars ppf = function
  | [] -> ()
  | s::[] -> fprintf ppf "'%s" s
  | s::rest -> fprintf ppf "'%s@ %a" s fmt_typvars rest

(* printer/parser functions *)

let std_writer f thing =
  f std_formatter thing
let str_writer f thing =
  f str_formatter thing; flush_str_formatter ()
let std_reader f () =
  f LambdaLex.tokenize (Lexing.from_channel stdin)
let file_reader f name =
  f LambdaLex.tokenize (Lexing.from_channel (open_in name))
let str_reader f s =
  f LambdaLex.tokenize (Lexing.from_string s)

let print_expr = std_writer fmt_expr
let print_value = std_writer (fmt_value false)
let print_typ = std_writer fmt_typ
let print_gentyp = std_writer fmt_gentyp

let str_of_expr = str_writer fmt_expr
let str_of_value = str_writer (fmt_value false)
let str_of_typ = str_writer fmt_typ
let str_of_gentyp = str_writer fmt_gentyp

let expr_from_stdin = std_reader LambdaParse.expr
let value_from_stdin = std_reader LambdaParse.value
let typ_from_stdin = std_reader LambdaParse.typ
let gentyp_from_stdin = std_reader LambdaParse.gentyp

let expr_from_file = file_reader LambdaParse.expr
let value_from_file = file_reader LambdaParse.value
let typ_from_file = file_reader LambdaParse.typ
let gentyp_from_file = file_reader LambdaParse.gentyp

let expr_from_str = str_reader LambdaParse.expr
let value_from_str = str_reader LambdaParse.value
let typ_from_str = str_reader LambdaParse.typ
let gentyp_from_str = str_reader LambdaParse.gentyp
