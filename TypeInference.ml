open Lambda;;
open LambdaUtils;;
open Format;;
open Substituion;;
open TypeEnv

exception TypeError = TypeEnv.TypeError;;

(*                    Debug use                             *)
let print_subs_e (e:subs_entry) =
	print_string (fst(e));
	print_string ";";
	print_typ (snd(e));
	print_string "\n";
	;;
let print_subs (s:subs) = 
	List.iter print_subs_e s
	;;
let print_env_e (e:env_entry) =
	 print_string (fst(e));
         print_string ";";
	 let var_list = fst(snd(e)) in
	 print_stringlist var_list;
	 print_string ";";
	 print_typ (snd(snd(e)))
	;;
let print_env (a:env) = 
	List.iter print_env_e a
	;;
	
(**************************************************************)	

let rec algw (env_a:env) (e:expr) : (subs * typ) = (
  match e with
  | Val(v) -> algw_val env_a v
  | Var(s) -> algw_var env_a s
  | Apply(e1, e2) -> algw_apply env_a e1 e2
  | If(p, e1, e2) -> algw_if env_a p e1 e2
  | Lambda(x, f) -> algw_lambda env_a x f
  | Let((x,f),g) -> algw_let env_a x f g
  | Letrec((x,f),g) -> algw_letrec env_a x f g
  )
  and algw_val env_a v = (
    match v with 
      | Number(n) -> ([],Int)
      | Boolean(b) -> ([],Bool)
      | Symbol(s) -> ([],Sym)
      | Pair(p1,p2) -> let (s1,t1) = algw env_a (Val(p1)) in 
                       let (s2,t2) = algw (applyToTypeEnv s1 env_a) (Val(p2)) in
                       let u = unify [] (List (applyToTypeExp s2 ([], t1))) t2 in
                          ([], applyToTypeExp u ([],t2))
      | Nil -> ([],List(new_tvar())) 
  )
  and algw_var env_a s = (
    if List.mem_assoc s env_a
    then ([], specialize s env_a)
    else raise (TypeError "Type checking failed ") 
  )
  and algw_apply env_a e1 e2 = (
    let (r, rou) = algw env_a e1 in
    let (s, deta) = algw (applyToTypeEnv r env_a) e2 in
    let beta = new_tvar() in
    let u = unify [] (applyToTypeExp s ([], rou)) (Fun(deta, beta)) in
    let tao = applyToTypeExp u ([], beta) in
    ((compose (compose u s) r), tao)
  )
  and algw_if env_a p e1 e2 = (
    let (r, rou) = algw env_a p in
    let u1 = unify [] rou Bool in
    let u1r = compose u1 r in
    let (s1, deta1) = algw (applyToTypeEnv u1r env_a) e1 in
    let s1u1r = (compose s1 u1r)  in
    let (s2, deta2) = algw (applyToTypeEnv s1u1r env_a) e2 in
    let u2 = unify [] (applyToTypeExp s2 ([],deta1)) deta2 in
    (compose (compose u2 s2) s1u1r, applyToTypeExp u2 ([], deta2))
  )
  and algw_lambda env_a x f = (
    let beta = new_tvar() in
    let (r, rou) = algw (add_entry (x,([],beta)) env_a) f in
    (r,applyToTypeExp r ([],Fun(beta, rou)))
  )
  and algw_let env_a x f g = (
    let (r, rou) = algw env_a f in
    let rou_pie = generalize x rou (applyToTypeEnv r env_a) in
    let (s, deta) = algw (applyToTypeEnv r (add_entry (x,rou_pie) env_a)) g in
    (compose s r, deta)
  )
  and algw_letrec env_a x e1 e2 = (
    let beta = new_tvar() in
    let (r, rou) = algw (add_entry (x,([],beta)) env_a) e1 in
    let u = unify [] (applyToTypeExp r ([],beta)) rou in
    let fix_t = compose u r in
    let rou1 = generalize x (applyToTypeExp fix_t ([],beta)) env_a in (*ax_rou1 is type of gentyp, please look at function gen 's def*)
    let (s, deta) = algw (applyToTypeEnv fix_t (add_entry (x,rou1) env_a)) e2 in
    (compose s fix_t, deta)
  );;

let typ_inference e = let (t, tao ) = algw init_typenv e in tao;;
