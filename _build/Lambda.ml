(**********************************************************************
 values are the primitives that can occur in an Extended-Lambda program
 Note: no List primitive, just Pair (a Pair whose second item is a List
       is a List, with base case (x . Nil))  The type rules for
       Extended-Lambda say that all Pairs must be (homogeneous) lists.
 **********************************************************************)
type value =
  | Number of int
  | Boolean of bool
  | Symbol of string
  | Pair of value * value
  | Nil

(**********************************************************************
 exprs are the expressions that can occur in an Extended-Lambda program
 we must implement function typ_inference to work on all possible
 exprs
 **********************************************************************)
type expr =
  | Val of value               (* literals including Pair *)
  | Var of string              (* IDs, including primitive fn "names" like + *)
  | If of expr * expr * expr   
  | Lambda of string * expr
  | Apply of expr * expr 
  | Let of (string * expr) * expr
  | Letrec of (string * expr) * expr

(**********************************************************************
 typs are the possible types of Extended-Lambda expressions.
 These are the possible return values of typ_inference for an
 expression that type-checks (if not, a TypeError exception is thrown).
 A substitution maps a type variable name (a string) to some typ.
 Note that there no type here for a pair, only for a List.
 **********************************************************************)
type typ = 
  | Int
  | Bool
  | Sym                   (* e.g., 'a *)
  | Tvar of string        (* e.g., 'a1 'a2 etc *)
  | List of typ
  | Fun of typ * typ

(**********************************************************************
 gentyp is a generic type: e.g., forall 'a, 'b. 'a-list -> 'b-list
 A type environment maps an ID (a value, defined above) to one of these
 (possibly with an empty "string list", if there is no "forall").
 Note that defining gentyp this way ensures that if there are quantifiers,
 they are only at the top level.
 **********************************************************************)
type gentyp = string list * typ

(**********************************************************************
 * typToString
 **********************************************************************)
let typToString ty =
 match ty with
 | Int -> "Int"
 | Bool -> "Bool"
 | Sym -> "Sym"
 | Tvar(s) -> "Tvar"
 | List(t) -> "List"
 | Fun(l, t) -> "Function"
