open Lambda;;
open LambdaUtils;;
open Format;;
open Substituion;;
open TypeEnv
exception TypeError of string (*I put it in Substituion.ml *)

(*let typ_inference e = raise (TypeError "Implement me.")*)

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
let rec intersect l1 l2 =
    match l1 with [] -> []
        | h1::t1 -> (
          match l2 with [] -> []
              | h2::t2 when h1 < h2 -> intersect t1 l2
              | h2::t2 when h1 > h2 -> intersect l1 t2
              | h2::t2 -> (
                match intersect t1 t2 with [] -> [h1]
                    | h3::t3 as l when h3 = h1 -> l
                    | h3::t3 as l -> h1::l
              )
        );;

let gen (x:string) (p:typ) (env_a:env) : gentyp =
	let p_free = freevars ([],p) in
	let nonfrees_env_a = if (List.mem_assoc x env_a)  then (fst (List.assoc x env_a)) else p_free in
	let common = intersect p_free nonfrees_env_a in
	(common, p)
	;;
(*testcase: let a = add_entry ("f", (["t1"], Fun(Tvar("t1"),Int))) init_typenv;;
gen "f" (Fun(Tvar("t1"),Fun((Tvar("t2"),Tvar("t3"))))) a;;*)  
let rec algw (env_a:env) (e:expr) : (subs * typ) =
	match e with
	|Val(v) -> let val_typ =  
		match v with 
		|Number(n) -> ([],Int)
		|Boolean(b) -> ([],Bool)
		|Symbol(s) -> ([],Sym)
		|Pair(p1,p2) -> let p1_subs_typ = algw env_a (Val(p1)) in   (*the notes say that lists are homogeneous; so Pair(p1,p2) 's type should be List(p1_typ). where p1_typ is p1's type; we need to raise a TypeError if p2's type is not List(p1's typ)*)
			        let p1_typ = snd(p1_subs_typ) in
				 ([], List(p1_typ))
		|Nil -> ([],List(Tvar(String.concat "" ["t";string_of_int((incr var_counter ; !var_counter))]))) (*When Val matches with Nil, a new type is generated and the type of Nil is List(the new type variable)*)
		in val_typ
	|Var(s) -> if (List.mem_assoc s env_a) then ([],spec s env_a)  else raise (TypeError "algw fail.")  (*  T is I,i.e., [] and tao is the result from function spec  if env_a has a key x; otherwise raise error   *) 
	
	|Apply(e1,e2) -> let (r, rou) = algw env_a e1 in
			 let (s, deta) = algw (applyToTypeEnv r env_a) e2 in
			 let beta = Fun(deta,Tvar(String.concat "" ["t";string_of_int((incr var_counter ; !var_counter))])) in
			 let u = unify [] (applyToTypeExp s ([],rou)) (Fun(deta, beta))  in
			 ((compose (compose u s) r), (applyToTypeExp u ([],beta)))
	|If(p,e1,e2) -> let (r, rou) = algw env_a p in
			let u1 = unify [] rou Bool in
			let u1r = compose u1 r in
			let (s1, deta1) = algw (applyToTypeEnv u1r env_a) e1 in
			let s1u1r = compose (compose s1 u1) r in
  			let (s2, deta2) = algw (applyToTypeEnv s1u1r env_a) e2 in
			let u2 = unify [] (applyToTypeExp s2 ([],deta1)) deta2 in
			let t = compose (compose (compose (compose u2 s2) s1) u1 ) r in 
			let tao = applyToTypeExp u2 ([], deta2) in
			(t, tao)
	|Lambda(x, f) -> let beta = Tvar(String.concat "" ["t";string_of_int((incr var_counter ; !var_counter))]) in
			 let ax_beta = add_entry (x,([],beta)) env_a in (*add_entry is defined in TypeEnv.ml*)
			 let (r, rou) = algw ax_beta f in
			 let t = r in
	    		 let tao = applyToTypeExp r ([],Fun(beta, rou)) in
			 (t, tao)
	
	|Let((x,f),g) -> let (r, rou) = algw env_a f in
			let rou_pie = gen x rou (applyToTypeEnv r env_a) in
			let (s, deta) = algw (applyToTypeEnv r (add_entry (x,rou_pie) env_a)) g in
			let t = compose s r in
			let tao = deta in
			(t, tao)
	;;

let typ_inference e = let (t, tao ) = algw init_typenv e in tao;;
