open Lambda ;;
open LambdaUtils ;;
open TypeInference ;;
open Format ;;

(* alpha_equal_typs: test equality up to alpha-reduction *)
let alpha_equal_typs (t1:typ) (t2:typ) =
  let map = ref [] in
  let rec typ_eq (t1:typ) (t2:typ) =
  match t1, t2 with
    | Int, Int | Bool, Bool | Sym, Sym -> true
    | Tvar a, Tvar b ->
      if List.mem_assoc a !map then (b = List.assoc a !map)
      else (map := (a,b) :: !map; true)
    | List s1, List s2 -> typ_eq s1 s2
    | Fun(arg1, ret1), Fun(arg2, ret2) ->
      List.for_all2 typ_eq (ret1::arg1::[]) (ret2::arg2::[])
    | _ -> false
  in
  typ_eq t1 t2
in

let fname1, fname2 = match Parseargs.parse_args 2 "Type equality checker." with
  | a :: b :: [] -> a,b
  | _ -> assert false
in
let t1 = try typ_from_file fname1 with
  | Parsing.Parse_error -> Format.printf "Parse error in %s.\n" fname1; exit 127
  | e -> Format.printf "Unhandled exception:\n"; raise e
in
let t2 = try typ_from_file fname2 with
  | Parsing.Parse_error -> Format.printf "Parse error in %s.\n" fname2; exit 127
  | e -> Format.printf "Unhandled exception:\n"; raise e    
in
if alpha_equal_typs t1 t2 then exit 0 else exit 1
