open Lambda;;
open LambdaUtils;;
open Format;;
open Substituion;;

(*exception TypeError of string*) (*I put it in Substituion.ml *)

let typ_inference e = raise (TypeError "Implement me.")

let var_counter = ref 0;;
 
let spec (x:string) (env_a:env) : typ =
	let x_gentyp = List.assoc x env_a in (*x_gentyp is the corresponding gentyp of x in env_a, i.e., environment A; I copied the def of env here: type env_entry = string * gentyp; type env = env_entry list*)
	let forall_vars = fst(x_gentyp) in (*a list of free_vars; the first part of gentyp, i.e., for example ["t1","t2"]*)
	let x_typ = snd(x_gentyp) in
	let rec get_subs_for_vars (vars_list: string list) : subs = 
	match vars_list with
	|[] -> []
	|v::left -> (v, Tvar(String.concat "" ["t";string_of_int((incr var_counter ; !var_counter))]))::(get_subs_for_vars left ) (*using Tvar(String.concat "" ["t";string_of_int((incr var_counter ; !var_counter))]) to generate a new typ Var!*)
	in 
	let forall_vars_subs = (get_subs_for_vars forall_vars) 
	in(*get the subsitution of the vars in forall*)	
	applyToTypeExp forall_vars_subs ([],x_typ)
	;;	
(*testcase: spec "isnil" init_typenv;;  *)	

let rec algw (env_a:env) (e:expr) : (subs * typ) =
	match e with
	|Val(v) -> 
		match v with 
		|Number(n) -> ([],Int)
		|Boolean(b) -> ([],Bool)
		|Symbol(s) -> ([],Sym)
		|Pair(p1,p2) -> ([], )
		|Nil -> ([],List(Tvar(String.concat "" ["t";string_of_int((incr var_counter ; !var_counter))]))) (*When Val matches with Nil, a new type is generated and they type of Nil is List(the new type variable)*)
	|Var(v) -> if (List.mem_assoc x env_a) then ([],spec x env_a)  else raise (TypeError "Implement me.")  (*  T is I,i.e., [] and tao is the result from function spec  if env_a has a key x; otherwise raise error   *) 
	
	|Apply(e1,e2) -> let (r, rou) = algw env_a e1 in
			 let (s, deta) = algw (applyToTypeEnv r env_a) e2 in
			 let beta = Fun(deta,Tvar(String.concat "" ["t";string_of_int((incr var_counter ; !var_counter))])) in
			 let u = unify [] (applyToTypeExp s ([],rou)) (Fun(deta, beta))  in
			 ((compose (compose u s) r), (applyToTypeExp u ([],beta)))
	
