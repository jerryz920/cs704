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
(*testcase:freevars ([],Fun(Tvar("t1"),Fun(Tvar("t2"),Int)));;
freevars (["t1"],Fun(Tvar("t1"),Fun(Tvar("t2"),Int)));;*)	
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
(*testcase: applyToTypeExp [("t1",Int)] ([],Fun(Tvar("t1"),Fun(Tvar("t2"),Int))) 
applyToTypeExp [("t1",Int)] (["t1"],Fun(Tvar("t1"),Fun(Tvar("t2"),Int)))*) 
let rec applyToTypeEnv (s:subs) (env_list:env) : env = 
	match env_list with
	|[] -> []
	|e::left -> let id = fst(e) in let ty = snd(e) in 
		let quantifiers = fst(ty) in let part = (id,(quantifiers, (applyToTypeExp s ty))) in part::(applyToTypeEnv s left)
	;;
(*testcase: applyToTypeEnv [("t1",Int)] init_typenv;;
applyToTypeEnv [("t1",Int)] (add_entry ("f",([],Tvar("t1"))) init_typenv)  *)
let compose (s1:subs) (s2:subs) : subs =
	let t = s2 in
	let rec apply_s1_to_t (x:subs) : subs=
	match x with 
	|[] -> []
	|e::left -> let variable = fst(e) in let typ_exp =snd(e) in 
			let part = (variable,(applyToTypeExp s1 ([],typ_exp))) 
			in part::(apply_s1_to_t left)
	in
	let applied_t = apply_s1_to_t t
	in
	let rec add_to_t (y:subs):subs=
	match y with
	|[] -> []
	|e1::left1 -> let typ_v1 = fst(e1) in 
	(*	if (List.mem_assoc typ_v1 applied_t) then (List.append applied_t (add_to_t left1)) else (List.append (e1::applied_t) (add_to_t left1))  *)
	if (List.mem_assoc typ_v1 applied_t) then (add_to_t left1) else 
		 ( e1::(add_to_t left1))
	in
	List.append (add_to_t s1) applied_t
	(*applied_t*)
	;;
(*testcase: compose [("t2",Int)] [("t1",Fun(Tvar("t2"),Int))];;
compose [("t2",Int);("t1",Int)] [("t1",Fun(Tvar("t2"),Int))];;
compose [("t2",Int);("t3",Int)] [("t1",Fun(Tvar("t2"),Int))]
let x = compose [("t1",Int)] [("t2",Tvar("t1"))];;
applyToTypeExp x ([],Tvar("t2"));; the result should be Int according the to on-line note*)

(* check if a variable occurs in a typ expression *)
let rec occurs (x : string) (t : typ) : bool =
  	match t with
  	|Int | Bool | Sym ->false
  	| Tvar(y) -> x = y
  	| List(e) -> occurs x e
	|Fun(e1,e2) -> (occurs x e1) || (occurs x e2)
	;;
(*testcase: occurs "t1" (Fun(Int,Tvar("t1")));;
occurs "t1" (List(Tvar("t1")));;
occurs "t1" (Fun(Int, List(List(Tvar("t1")))));;
*)
(*
let isTvar (exp:typ) : bool =
	match exp with 
	|Tvar(str) -> true
	|_ -> false
	;;
*)
let get_Tvar_t (exp:typ) : (bool * string) = (*judge whether a typ expression is of type Tvar, this function also return the string t  in Tvar(t)*)
	match exp with
	|Tvar(t) -> (true, t)
	|_ -> (false, "")
	;;
(*testcase: get_Tvar_t (Tvar("t1"));; *)
let rec unify (s:subs) (expr1:typ) (expr2:typ) : subs =
	let t1 = get_Tvar_t(expr1) in
	let t2 = get_Tvar_t(expr2) in 
	let bool_t1 = fst(t1) and string_t1 = snd(t1) in
	let bool_t2 = fst(t2) and string_t2 = snd(t2) in
	if bool_t1 then 
