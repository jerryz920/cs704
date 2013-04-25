type atom =
  | Numeral of int
  | Symbol of string
  | SExpr of expr
  | Improper of atom list * atom
  | Colon
  | Arrow
and expr = atom list
and program = atom list

