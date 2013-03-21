open Lambda ;;
open TypeEnv ;;
open LambdaUtils;;
open Format;;

type subs = (string * typ) list (*subs is a list of pair (type-variable,unquantified-type-expressions), that is, a list of (string,'typ')*)
exception TypeError of string  (*I put TypeError here *)

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
let get_Tvar_t (exp:typ) : (bool * string) = (*judge whether a typ expression is of type Tvar, this function also return the string(type variable) t  in Tvar(t)*)
	match exp with
	|Tvar(t) -> (true, t)
	|_ -> (false, "")
	;;
(*testcase: get_Tvar_t (Tvar("t1"));; *)
let root (exp:typ): string = (*exp must NOT be type of Tvar(something)*) 
	match exp with
	|List(e) -> "List"
	|Fun(e1,e2) -> "Fun"
	|Int|Bool|Sym -> "Primitive"
	;;
(*testcases: root (Fun(Int,Tvar("t1")));;
root (List(Tvar("t1")));;
root Int;; root Bool
root Int = root Bool;;*)
let rec unify (s:subs) (expr1:typ) (expr2:typ) : subs =
	let tuple_t1 = get_Tvar_t(expr1) in  (*t1 is a tuple (bool,string)*)
	let tuple_t2 = get_Tvar_t(expr2) in 
	let bool_t1 = fst(tuple_t1) and string_t1 = snd(tuple_t1) in (*string_t1 is equal to t of the on-line noew in concept*)
	let bool_t2 = fst(tuple_t2) and string_t2 = snd(tuple_t2) in
	
	let bool_s_has_key_t1 = (List.mem_assoc string_t1 s) in (*to see whether s has a <key,value> mapping with t1(string) as the key*)
	if bool_t1 then                                       (*if (exp1 is TYPEVAR(t))*) 
			(if (bool_t2 && string_t1 = string_t2) then s (*if (exp2 is also TYPEVAR(t)  then s*)
			else (if (occurs string_t1 expr2) then (raise (TypeError "unify fail."))   (*else if (t occurs in exp2) return FAIL*)
			     else (if  bool_s_has_key_t1 then (unify s (List.assoc string_t1 s) expr2) (*	else if (S maps t to some type expression e) return Unify(S, e, exp2)
													 ;s is type of subs; so (List.mem_assoc string_t1 s) is type if typ*)
			          else (let expr2_dot = applyToTypeExp s ([],expr2) in (*	else let exp2' = S(exp2) in*) 
				       let expr2_dot_t = get_Tvar_t(expr2_dot) in
				       let bool_expr2_dot_t = fst(expr2_dot_t) and string_expr2_dot_t = snd(expr2_dot_t) in
				           (if (bool_expr2_dot_t && string_expr2_dot_t = string_t1) then s (*if (exp2' is TYPEVAR(t)) return S *)
				           else if (occurs string_t1  expr2) then (raise (TypeError "unify fail.")) (*else if (t occurs in exp2') return FAIL*)
				                else (compose [(string_t1, expr2_dot)] s) (*else return t:exp2' o S ; please note that function compose requires two parameters with type of subs*)
					   )
				      )
				  )
			    )
			) (*coressponding to if*)
	(*else if bool_t2 then (unify expr2 expr1) (*if (exp2 is TYPEVAR(t)) return unify(S, exp2, exp1) *) 
	(*here if neither exp1 nor exp2 is a type variable, i.e., is not type of Tvar(something)*)
	else if (not (root (expr1) = root (expr2))) then (raise (TypeError "unify fail.")) (*if (root(exp1) ≠ root(exp2)) return FAIL*)
	else if ( (root (expr1) = "Primitive" ) && (root (expr2) = "Primitive" ) ) then s (*if (root(exp1) and root(exp2) are primitive types)  return s*)
	else if ( (root (expr1) = "Fun")) then let next_sub =
						match expr1, expr2 with
						|Fun(e1,e2),Fun(e3,e4) -> let new_sub = unify s e1 e3 in unify new_sub e2 e4
				                in next_sub 
	*)
	else s
	;;	
