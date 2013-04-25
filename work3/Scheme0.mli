(****** The regular Scheme0 abstract syntax tree ******)

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

(* Function definition: Parameter names, and expression using those bindings. *)
and definition = string list * expr
(* Program: Associative list. Maps function names to definitions.
   The goal function is named "main". *)
and program = (string * definition) list

(****** Binding-time Types ******)

(* bind time: Static (S) or Dynamic (D). *)
type bind_time = S | D
(* bind time environment: bind times of parameters and the function's value. *)
type bind_time_env = (string * bind_time) list * bind_time
(* division: Association list. Maps function names to bind-time environments. *)
type division = (string * bind_time_env) list

(****** Exceptions ******)
(* A SyntaxError indicates a syntax error in the input Scheme0 program. *)
exception SyntaxError of string
(* A UtilityError indicates an implementation error in Scheme0Utils. *)
exception UtilityError 
(* An EvalError indicates an evaluation error of a Scheme0 program. *)
exception EvalError of string

(****** Input and Output ******)    
val print_expr: expr -> unit
val print_program: program -> unit
val print_value: value -> unit
val print_division: division -> unit

val string_of_value: value -> string
val string_of_expr: expr -> string
val string_of_program: program -> string
val string_of_division: division -> string

val program_from_file: string -> program
val division_from_file: string -> division

val value_from_string: string -> value

(****** residue_name: determines the name of a specialized function. ******)
val residue_name: string -> value list -> string

(****** The pieces of the partial evaluator. You'll implement these. ******)
val bind_time_analysis: program -> division
val specialize: program -> division -> program
