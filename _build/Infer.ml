open LambdaUtils ;;
open TypeInference ;;
open Format ;;

let fname = match Parseargs.parse_args 1 "Type inference driver." with
  | f :: [] -> f
  | _ -> assert false in

try
  let e = expr_from_file fname in
  let t = typ_inference e in
  print_typ t; print_newline ();
with 
  | Parsing.Parse_error ->
    Format.printf "Parse error in %s.\n" fname
  | TypeError s ->
    Format.printf "Type checking failed for %s:\n%s\n" fname s
  | e -> Format.printf "Unhandled exception:\n"; raise e    
;;
