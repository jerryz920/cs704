open Lambda ;;

(* Data structures to text *)

val print_expr: expr -> unit
val print_value: value -> unit
val print_typ: typ -> unit
val print_gentyp: gentyp  -> unit

val str_of_expr: expr -> string
val str_of_value: value -> string
val str_of_typ: typ -> string
val str_of_gentyp: gentyp -> string

(* Text to data structures *)

val expr_from_stdin: unit -> expr
val value_from_stdin: unit -> value
val typ_from_stdin: unit -> typ
val gentyp_from_stdin: unit -> gentyp

val expr_from_file: string -> expr
val value_from_file: string -> value
val typ_from_file: string -> typ
val gentyp_from_file: string -> gentyp

val expr_from_str: string -> expr
val value_from_str: string -> value
val typ_from_str: string -> typ
val gentyp_from_str: string -> gentyp

(* Generic, "user-defined printers". You probably don't want to use
 * these; unless you want to make your own custom printers. For more
 * details, see:
 * http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html *)

val fmt_expr: Format.formatter -> expr -> unit
(* fmt_value's first arg should be true iff this value is in a quotation. *)
val fmt_value: bool -> Format.formatter -> value -> unit
val fmt_typ: Format.formatter -> typ -> unit
val fmt_gentyp: Format.formatter -> gentyp  -> unit
