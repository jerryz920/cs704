open Lambda ;;
open TypeEnv ;;

type subs = (string * typ) list (*subs is a list of pair (type-variable,unquantified-type-expressions), that is, a list of (string,'typ')*)

let rec freevars (t:gentyp) : string list = 
	let non_frees = fst(t) in
	let typ_e = snd(t) in 
	match typ_e with
	Int 	->[]
	|Bool  -> []
	|Sym  -> []
	|Tvar(tl) -> if List.mem tl non_frees then [] else [tl]
	|List(tl) -> (freevars (non_frees, tl))
	|Fun(tl,tr) ->List.append (freevars (non_frees, tl)) (freevars (non_frees, tr))
	;;
(*testcase:freevars ([],Fun(Tvar("t1"),Fun(Tvar("t2"),Int)));;*)	
let rec applyToTypeExp (s:subs) (gentyp_exp:gentyp) : typ =
	let frees = freevars gentyp_exp in
	let typ_e = snd(gentyp_exp) in
	match typ_e with
	|Int -> typ_e
	|Bool -> typ_e
	|Sym -> typ_e
	|Tvar(tl) ->if (List.mem tl frees && List.mem_assoc tl s) then List.assoc tl s else typ_e(*if tl is free variable in gentyp_exp && there is a binding in s for tl*)
	|List(tl) -> List (applyToTypeExp s (fst(gentyp_exp),tl))
	|Fun(tl,tr) -> Fun((applyToTypeExp s (fst(gentyp_exp),tl)),(applyToTypeExp s (fst(gentyp_exp),tr)))
	;;
(*testcase: applyToTypeExp [("t1",Int)] ([],Fun(Tvar("t1"),Fun(Tvar("t2"),Int))) *) 
let rec applyToTypeEnv (s:subs) (env_list:env) : env = 
	match env_list with
	|[] -> []
	|e::left -> let id = fst(e) in let ty = snd(e) in 
		let quantifiers = fst(ty) in let part = (id,(quantifiers, (applyToTypeExp s ty))) in part::(applyToTypeEnv s left)
	;;
(*testcase: applyToTypeEnv [("t1",Int)] init_typenv;;
applyToTypeEnv [("t1",Int)] (add_entry ("f",([],Tvar("t1"))) init_typenv)  *)
