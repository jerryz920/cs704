open Lambda ;;
open TypeEnv ;;
open LambdaUtils;;
open Format;;

type subs = (string * typ) list 
type subs_entry = string * typ;;

let var_counter = ref 0;;
let new_tvar () = Tvar(String.concat "" ["t";string_of_int((incr var_counter ; !var_counter))])

(* Type Env operations *)
let rec freevars (t:gentyp) : string list = 
  let (non_frees,typ_e) = t
  in match typ_e with
      Int | Bool | Sym  -> []
      |Tvar(tl) -> if List.mem tl non_frees then [] else [tl]
      |List(tl) -> (freevars (non_frees, tl))
      |Fun(tl,tr) ->List.append (freevars (non_frees, tl)) (freevars (non_frees, tr))
;;

let occurs (x : string) (t : typ) : bool =
  List.mem x (freevars ([], t))
;;

let rec applyToTypeExp (s:subs) (exp:gentyp) : typ =
  let (var_e, typ_e) = exp in
    match typ_e with
      Int |Bool |Sym -> typ_e
      |Tvar(tl) ->(if (List.mem tl (freesvars exp) && List.mem_assoc tl s)
                   then List.assoc tl s 
                   else typ_e
                  )
      |List(tl) -> List(applyToTypeExp s (var_e, tl))
      |Fun(tl,tr) -> Fun((applyToTypeExp s (var_e, tl)),(applyToTypeExp s (var_e, tr)))
;;

let applyToTypeEnv (s:subs) (env_list:env) : env = 
  List.map (fun (id, (x, v)) -> (id,(x,applyToTypeExp s (x,v)))) env_list
;;

let compose (s1:subs) (s2:subs) : subs =
  let new_t = List.map (fun (k2, v2) -> (k2, applyToTypeExp s1 ([],v2))) s2
  and new_ents = List.find_all (fun (k1, _) -> not (List.mem_assoc k1 s2)) s1
  in new_t @ new_ents


let specialize (x:string) (env_a:env) : typ =
  let (forall_vars,x_typ) = List.assoc x env_a
   in applyToTypeExp (List.map (fun v -> (v, new_tvar())) forall_vars) ([],x_typ)
;;

let generalize (x:string) (p:typ) (env_a:env) : gentyp =
  let difference l1 l2 = List.find_all (fun x -> not (List.mem x l2)) l1
  and vars_in_env env_a = 
    List.fold_left (fun res (_,obj) -> (freevars obj) @ res) [] env_a
  in (difference (freevars ([], p)) (vars_in_env env_a), p);;

(* Unify *)
let rec unify (s:subs) (expr1:typ) (expr2:typ) : subs = 
        match expr1, expr2 with
         | Tvar(x), Tvar(y) when x == y -> s
         | Tvar(x), _ when occurs x expr2 -> raise (TypeError "Unify fail1")
         | Tvar(x), _ when List.mem_assoc x s -> unify s (List.assoc x s) expr2
         | Tvar(x), _ -> (let expr2' = applyToTypeExp s ([], expr2)
                          in match expr2' with
                          | Tvar(y) when x == y -> s
                          | _ when occurs x expr2' -> raise (TypeError "Unify fail2")
                          | _ -> compose [(x, expr2')] s
                         )
         | _, Tvar(_) -> unify s expr2 expr1
         | Int, Int | Bool, Bool | Sym, Sym -> s
         | Fun(e1, e2), Fun(e3, e4) -> unify (unify s e1 e3) e2 e4
         | List(ex1), List(ex2) -> unify s ex1 ex2
         | _, _ -> raise (TypeError "Unify fail 3")
;;

(*testcase1 : expr1 is Fun(Int, Bool); expr2 is Fun(Tvar("t1"),Bool); s =[] so  ----unify [] (Fun(Int, Bool)) (Fun(Tvar("t1"),Bool));; the returned result is corrent: subs = [("t1", Int)]*)
(*testcase2:  expr1 is Fun(Tvar("t1"),Fun(Tvar("t2"),Int)); expr2 is Fun(Int, Fun(Bool, Tvar("t1"))); s = [], so unify [] (Fun(Tvar("t1"),Fun(Tvar("t2"),Int))) (Fun(Int, Fun(Bool, Tvar("t1"))));; the result is correct: subs = [("t2", Bool); ("t1", Int)]  *)
(*testcase3: expr1 is Fun(Tvar("t1"), Fun(Tvar("t2"),Bool)); expr2 is Fun(Int, Tvar("t3")); s is [] so --- unify [] (Fun(Tvar("t1"), Fun(Tvar("t2"),Bool))) (Fun(Int, Tvar("t3")));; the result is corrent :subs = [("t3", Fun (Tvar "t2", Bool)); ("t1", Int)]   *)

(*testcase4: expr1 is (Fun(Tvar("t1"),Fun(Tvar("t2"),Bool))); expr2 is (Fun(Int,Tvar("t1"))); s is [], 
so ---unify [] (Fun(Tvar("t1"),Fun(Tvar("t2"),Bool))) (Fun(Int,Tvar("t1")));; the result is corrent: Exception: TypeError "unify fail."     *)
(*testcase5: expr1 is (Fun(Tvar("t1"),Fun(Tvar("t2"),Bool))); expr2 is (Fun(Tvar("t2"),Tvar("t1"))); s is [], so ---unify [] (Fun(Tvar("t1"),Fun(Tvar("t2"),Bool))) (Fun(Tvar("t2"),Tvar("t1")));; the result is corrent  *)
(*testcase6; expr1 is (List(Tvar("t1"))) ; expr2 is (List(Int)); s =[], so ----unify [] (List(Tvar("t1"))) (List(Int));;   result correct *)
(*testcase7: expr1 is (List(Fun(Int, Bool))); expr2 is (List(Fun(Tvar("t1"),Bool))); s = [], so ---unify [] (List(Fun(Int, Bool))) (List(Fun(Tvar("t1"),Bool)))  ;; result correct *)   
