include Scheme0Utils;;
open Format;;
(*implement union *)
let union (b1:bind_time) (b2:bind_time):bind_time =
	if b1 == S && b2 == S then S else D
	;;
(*implement B_e *)
type btenv = (string * bind_time) list;;

type div = (string * btenv) list;; (*Note that div != division, div is an intermediate result*)
type fn_bte = (string * btenv);;
type btenv_list = btenv list;;
type fn_expr = (string * expr) list;; (*function name and expr : list*)


let rec bind_expr (e:expr) (tao:btenv) (d:div) :bind_time =
	match e with 
	|Val(v) -> S
	|Var(v) -> List.assoc v tao
	|If(e1,e2,e3) -> let r1 =  bind_expr e1 tao d in
			 let r2 = bind_expr e2 tao d in
			 let r3 = bind_expr e3 tao d in
			 union r1 (union r2 r3)
	|Call(s,l) -> 
		      let s_btenv = List.assoc s d in
		      let s_bindings = List.map snd s_btenv in
		      let s_type = List.fold_left (union) S s_bindings in
		      if s_type=D then D else 
		      (
		      let l1 = List.map (fun x -> bind_expr x tao d) l in (*bind_expr e_i tao, where e_i is the ith element of list l (recall that l is a list of expr).l1 is a list of bind_time*)
		      let r = List.fold_left (union) S l1 in   (* union each element of list l1; if any one of the elements is D, then r is D, else r is S*)
	   	      r
		      )
	|Binop(bin, e1, e2) -> union (bind_expr e1 tao d) (bind_expr e2 tao d)
	|Unop(un, e1) -> (bind_expr e1 tao d)
	;;

let list_union (bt1:btenv) (bt2:btenv):btenv =
	(*print_string "hello\n";*)
	List.map2 (fun x y -> (fst(x),union (snd(x)) (snd(y)) ) ) bt1 bt2
	;;
let rec bind_expr_fn (e:expr) (tao:btenv) (g:string) (gtao:btenv) (d:div) (if_dynamic:bool): btenv = (*tao is function f_i's tao and e is function f_i's expression*)
	(*the return is function g's update tao, tao is type of btenv*)
	match e with
	|Val(v) -> List.map (fun x -> (fst(x),S)) gtao 
	|Var(v) -> List.map (fun x -> (fst(x),S)) gtao
	|If(e1,e2,e3) -> (*consider dynamic if*)
		         let condition_type = bind_expr e1 tao d in
	                 let is_dynamic= condition_type = D in
			 let r1 = bind_expr_fn e1 tao g gtao d is_dynamic in
			 let r2 = bind_expr_fn e2 tao g gtao d is_dynamic in
			 let r3 = bind_expr_fn e3 tao g gtao d is_dynamic in
			 
			 list_union r1 (list_union r2 r3) 
	
	|Call(f,l) ->  if (if_dynamic) then (
				if f=g then (List.map (fun x -> (fst(x),D)) gtao)
				else (List.map (fun x -> (fst(x),S)) gtao)
				) (*only f=g (they are the same function, i.e., dynamic if inlcude the g)*)
		       else
		       ((*the call is not in dynamic if*)
		       let l1 = List.map (fun x -> bind_expr_fn x tao g gtao d if_dynamic) l in (*bind_expr_fn e_i tao g, where e_i is the ith element of list l (recall that l is a list of expr).Note that l1 is a list of btenv*)
		       (*the btenv in l1 should be the same type as gtao*)
		       
		       let list_static = List.map (fun x -> (fst(x),S)) gtao in
		       let t = List.fold_left (list_union) list_static l1 in (*the element of l1 is also a list:btenv*)
		       if not(f=g) then t (*short circurt*) 
	
		       else 
		       ( let list_after_call_bind_expr = List.map2 (fun x y -> (fst(x), bind_expr y tao d)) gtao l in (*the order of tao and l should be same*)
			 list_union t list_after_call_bind_expr
		       )
		       ) (*the call is not in dynamic if*)
	
	|Binop(bin, e1, e2) -> list_union (bind_expr_fn e1 tao g gtao d if_dynamic) (bind_expr_fn e2 tao g gtao d if_dynamic)
	
	|Unop(un, e1) -> (bind_expr_fn e1 tao g gtao d  if_dynamic)
	
	;;
(*
	1.
	let e = Call("f",[Call("g",[Val(Number(0))])]);;
	let tao = [("a",S)];;
	let g= "f";;
	let gtao = [("a",S)];;
	let d = [("g",[("x",D)]) ];;
	bind_expr_fn e tao "f" gtao d;;
	2.
	let e1 = Binop(Plus, Call("sumunder",[Var("n");Val(Number(3))]),  Call("sumunder",[Var("n");Val(Number(25))]));;
	let e2 = Call("sumunder",[Var("n");Val(Number(15))]);;
	let e = Binop(Minus, e1, e2);;
	let tao = [("n",D)];;
	let g = "sumunder";;
	let gtao = [("n",S) ; ("k",S)];;
	let d = [("main",[("n",D)]) ];;
	bind_expr_fn e tao "sumunder" gtao d;;
	////////////////////////////
	let e1 = Binop(Minus, Var("n"), Var("n"));;
	let e2 = Var("k");;
	let e3 = Var("n");;
	let e = Call("sumbetween", [e1; e2; e3]);;
	let tao = [("n",D); ("k",S)];;
	let gtao = [("start",S) ; ("step",S); ("end",S)];;
	let d = [("sumunder", [("n",D); ("k",S)]); ("sumbetween", [("start",S) ; ("step",S); ("end",S)]) ];;
	bind_expr_fn e tao "sumbetween" gtao d;;
	/////////////////////////
	let e1 = Binop(LessThan, Var("start"), Var("end"));;
	let e2 = Var("start");;
	let e3 = Call("sumbetween", [Var("start"); Var("end"); Var("step")]);;
	let e = If(e1,e2,e3);;
	let tao = [("start",D) ; ("step",D); ("end",D)];;
	let gtao = [("n",D); ("k",S)];;
	let d = [("sumunder", [("n",D); ("k",S)]); ("sumbetween", [("start",D) ; ("step",D); ("end",D)]) ];;
	bind_expr_fn e tao "sumunder" gtao d false;; 
	
*)
let compare (d1:div) (d2:div) : bool = (*if d1 and d2 are the same return true else flase*)
	let names = List.map fst d1 in
	let equal_each (name:string) : bool = (List.assoc name d1) = (List.assoc name d2) 
	in
	let all_bools = List.map equal_each names in
	List.fold_left (&&) true all_bools
	;;

(*find least dynamic div*)
(*one import thing is that the order of the functions in div might be different for different divs; but given a certian function f, then in f's btenv, the formals order is always the same*)
let rec find_least (d:div) (fe:fn_expr) :div = 
	(*this function corresponds to bind_expr_fn e_i (div f_i) g where i is the ith function*)
	let func_i_impacts (fb:fn_bte) (g:string) : btenv = 
	(*fb is the basic element of list d*) 
		let name = fst(fb) in (*name of the function, i.e., f_i*)
		let gtao = List.assoc g d in  (*this is function g 's tao, NOT f_i 's tao*)
	   	bind_expr_fn (List.assoc name fe) (List.assoc name d) g gtao d false(*List.assoc name fe==expr; List.assoc name d==btenv*)
	in
	(*this function try to gather the impacts of all functions on g, it returns a list of btenv*)
		let func_all_impacts (g:string) : btenv_list = List.map (fun x -> func_i_impacts x g) d (*x is type of fn_bte; d is a list of x(d is a list of fn_bte)*)
	in 
		let g_tao (g:string) :btenv = List.assoc g d 
	in
		let list_static (g:string) :btenv = List.map (fun x -> (fst(x),S)) (g_tao g) 
	in
		(*this function try to union the impacts of the list returned by func_all_impacts on g*)
		let union_func_all_impacts (g:string) :btenv = List.fold_left (list_union)  (list_static g) (func_all_impacts g) 
	in
		let get_out_of_main = List.filter (fun x -> not(fst(x)="main")) d 
	in (*get_out_of_main is type of div, but it does not include main's information*)
		let get_out_of_main_names = List.map fst get_out_of_main 
	in (*only function names, a string list*)
		(*update the functions (except for main) 's btenv)*)
		let newdiv_wo_main = List.map (fun x -> (x,union_func_all_impacts x)) get_out_of_main_names 
	in
		let main_info = [("main",(List.assoc "main" d))] 
	in
		let newdiv = List.append main_info newdiv_wo_main 
	in
		if compare newdiv d then d else (find_least newdiv fe)
	;;
(*
	let d0 = [("main",[("x",D)]); ("foo",[("a",S)]); ("bar",[("x",S)]); ("f",[("a",S)]); ("g",[("x",S)])];;
	let fe = [ ("main", Binop(Plus,Call("foo",[Val(Number(1))] ), Call("bar", [Var("x")] ) ) ) ;
	          ("foo", Call("f",[ Call("g", [Val(Number(0))]) ] ));
		  ("bar", Call("g", [Var("x")]) ) ;
		  ("f", Var("a")) ;
		  ("g", Val(Number(20)))
		 ];;
	bind_expr_fn d0 fe;; (correct)
*)
	
let init_div (prog:program) : div =
	let get_btenv ( str_def:(string*definition) ) : (string*btenv) =
		let name = fst(str_def) 
		in
		let def = snd(str_def) (*str is the name of a function, def is the definition of that function*)
		in
		let formal_list = fst(def) 
		in
		let bt = if name="main" then List.map (fun x -> (x,D)) formal_list else List.map (fun x -> (x,S)) formal_list
		in (name,bt)
	in
	List.map get_btenv prog
	;;
		
(* Implement these. *)
let bind_time_analysis (prog:program) :division = 
	let div0 = init_div prog
	in
	let fe = List.map (fun x -> (fst(x), snd(snd(x))) ) prog  (*get a list of function_name (string) * expr*)
	in
	let main_formals = List.map fst (List.assoc "main" div0)
	in
	let ismain0_value = List.length main_formals=0 
	in
	(**let final_div = if ((List.length main_formals=0)) then div0 else (find_least div0 fe)  *)
	let final_div = find_least div0 fe
	(*let final_div = find_least final_div_0 fe *)
	in
	let get_divison (d:div) (name_expr:fn_expr) : division =
		let get_bind_time_env (str_btenv: (string*btenv)) (ismain0:bool) : bind_time_env =
			
			let name = fst(str_btenv)  (*the name of a function, it is a string*)
			in 
			let bt = snd(str_btenv)    (*the btenv of a function, note the div is a list of (string*btenv) *)
			in
			if ismain0 then (if name="main" then (bt,D) else (bt,S))
			else 
			(
			let bindings = List.map snd bt   (*bindings is a list. it looks like (S,D,D....) *)
			in
			let has_dynamic = List.exists (fun x -> x=D) bindings   (*fixing this point--if any formal of a function is Dynamic, then the fucntion's type is also Dynamic*)
			in 
			let _ = bind_expr (List.assoc name name_expr) bt d
			in
			if has_dynamic then (bt,D) else
			(*
			(bt,expr_type)
			*)(*!!!!!!!!!!!!!!!*)
			(bt,S)
			)
		in
		List.map (fun x -> (fst(x), get_bind_time_env x ismain0_value)) d
	in
	get_divison final_div fe
	;;

(* the pending functions *)
type env = (string * expr) list
type penv = (string * expr option) list

  (*
   * Some utility functions
   *)

let make_env name_list val_list: env = List.combine name_list val_list
;;

let part_env name_list val_list (fdiv: bind_time_env) = 
  let total_env = make_env name_list val_list
  and arg_div = fst fdiv
  in let collect (k,v) (s_env, d_env) =
    (
      if List.assoc k arg_div == S then
        ((k,v) :: s_env, d_env)
      else
        (s_env, (k,v) :: d_env)
    ) in
    List.fold_right collect total_env ([],[]) 
;;

let get_names env_list = List.map (fun (k,_) -> k) env_list
;;
let get_vals env_list = List.map (
  fun (k,v) -> match v with 
    | Val(x) -> x
    | _ -> raise (EvalError "can not evaluate non value expr")
  ) env_list
;;
let get_expr_vals env_list = List.map (fun (k,v) -> v) env_list
;;

let get_fdiv fname div prog =
  if List.mem_assoc fname div then
    List.assoc fname div
  else
    let arglist = fst(List.assoc fname prog)
    in (List.map (fun n -> (n, D)) arglist, D)
;;

let rec print_env_list l =
  if List.length l > 0 then
  (print_string (fst (List.hd l)); print_string "="; print_expr (snd (List.hd l)); print_string " "; print_env_list (List.tl l); print_string "\n")
  else
    print_string "\n"
;;

let rec print_expr_list l =
  if List.length l > 0 then
    (print_expr (List.hd l); print_string ","; print_expr_list (List.tl l);
    print_string "\n";)
  else
    print_string "\n"
;;

(*
 *      We ensure only expr inside
 *)
let eval_expr expr: expr =
  let eval_bin_expr op v1 v2 =
    (
      match op, v1, v2 with
      | Cons, _, _ -> Pair(v1,v2)
      | Plus,(Number i1),(Number i2) -> Number (i1 + i2)
      | Minus,(Number i1),(Number i2) -> Number (i1 - i2)
      | Times,(Number i1),(Number i2) -> Number (i1 * i2)
      | Div,(Number i1),(Number i2) -> Number (i1 / i2)
      | Mod,(Number i1),(Number i2) -> Number (i1 mod i2)
      | Equals,(Number i1),(Number i2) -> Boolean (i1 = i2)
      | Equals,(Boolean b1),(Boolean b2) -> Boolean (b1 = b2)
      | Equals,(Symbol s1),(Symbol s2) -> Boolean (s1 = s2)
      | Equals,(Pair (pv1,pv2)),(Pair (pv3,pv4)) -> Boolean (pv1 = pv3 && pv2 = pv4)
      | Equals,_,_ -> Boolean(false)
      | LessThan,(Number i1),(Number i2) -> Boolean (i1 < i2)
      | LessThan,(Symbol s1),(Symbol s2) -> Boolean (s1 < s2)
      | GreaterThan,(Number i1),(Number i2) -> Boolean (i1 > i2)
      | GreaterThan,(Symbol s1),(Symbol s2) -> Boolean (s1 > s2)
      | _,_,_ -> raise (EvalError "can not evaluate bin expr")
    )
  and eval_un_expr op v =
    (
      match op,v with
      | Car,Pair(v1,_) -> v1
      | Car,Nil -> Nil
      | Cdr,Pair(_,v2) -> v2
      | Cdr,Nil -> Nil
      | IsNil,Nil -> Boolean (true)
      | IsNil,_ -> Boolean (false)
      | _,_ -> raise (EvalError "can not evaluate un expr")
    )
  in match expr with
   | Binop(op, Val(v1), Val(v2)) -> Val (eval_bin_expr op v1 v2)
   | Unop(op, Val(v)) -> Val (eval_un_expr op v)
   | _ -> raise (EvalError "can not evaluate non-op expr")
;;


  (*
   * Do we check for type errors? Like undefined variables? Assume no
   *)
let rec reduce e prepared_env prog div: (expr * 'residual_functype list)=
  let reduce_var x exp_env = 
    (match (List.assoc x exp_env) with
        | Some v ->  (v, [])  (* Static value *)
        | None ->  (Var(x), []) (* Dynamic, return itself *)
    )
  and reduce_if e1 e2 e3 exp_env prog div =
    (
      let (reduced1, pending1) = reduce e1 exp_env prog div
      and branch2 = lazy (reduce e2 exp_env prog div)
      and branch3 = lazy (reduce e3 exp_env prog div)
      in match (reduced1) with
        | Val Boolean(true) -> Lazy.force branch2
        | Val Boolean(false) -> Lazy.force branch3
        | _ -> let (reduced2,pending2) = Lazy.force branch2
               and (reduced3,pending3) = Lazy.force branch3
               in (If (reduced1, reduced2, reduced3), pending1 @ pending2 @ pending3)
    )
  and reduce_bin op e1 e2 exp_env prog div =
    (
      let (reduced1, pending1) = reduce e1 exp_env prog div
      and (reduced2, pending2) = reduce e2 exp_env prog div
      in match(reduced1,reduced2) with
        | (Val(v1),Val(v2)) -> (eval_expr (Binop(op, reduced1, reduced2)), [])
        | _ -> (Binop(op, reduced1, reduced2), pending1 @ pending2)
    )
  and reduce_un op e1 exp_env prog div =
    (
      let (reduced1, pending1) = reduce e1 exp_env prog div
      in match(reduced1) with
        | Val(v1) -> (eval_expr (Unop(op, reduced1)), [])
        | _ -> (Unop(op, reduced1), pending1)
    )
  and reduce_call fname args exp_env prog div =
    (
      let (argnames, fbody) = List.assoc fname prog
      and reduce_arg arg (argvals,pending_funcs) =
        (
          let (reduced_arg, new_pending_funcs) = reduce arg exp_env prog div
          in (reduced_arg :: argvals, pending_funcs @ new_pending_funcs)
        )
      in
        (
      (*
       * We first reduce the arguements to simpliest form
       * Then we generate the environment for this call
       *
       * If this call is static, we reduce the body with given environment
       * If this call is dynamic, we generate a Call with proper arguments
       *   and insert a new function to pending list
       *)
        let (argvals,pending_funcs) = List.fold_right reduce_arg args ([],[])
        and fdiv = get_fdiv fname div prog
        in
          (
            let (s_env, d_env) = part_env argnames argvals fdiv
            in
              (let prepared_senv = List.map (fun (k,v) -> (k, Some v)) s_env
               and prepared_denv = List.map (fun (k,v) -> (k, None)) d_env
               in match (snd fdiv) with
                 | S -> reduce fbody (prepared_senv @ prepared_denv) prog div
                 | D -> 
                   (
                     (* Note we actually should check if this name already there *)
                     let new_name = residue_name fname (get_vals s_env)
                     in 
                       (
                         Call (new_name, get_expr_vals d_env),
                         (new_name, prepared_senv, prepared_denv, fbody)::pending_funcs
                       )
                   )
              )
          )
        )
    )
  in
  match e with
    | Val(v) -> (Val(v), [])
    | Var(x) -> (reduce_var x prepared_env)
    | Call(fname, args) -> (reduce_call fname args prepared_env prog div)
    | If(e1,e2,e3) -> (reduce_if e1 e2 e3 prepared_env prog div)
    | Binop(op,e1,e2) -> (reduce_bin op e1 e2 prepared_env prog div)
    | Unop(op, e1) -> (reduce_un op e1 prepared_env prog div)
;;

let rec specialize_func (pending: (string * penv * penv * expr) list) prog div processed =
  if List.length pending = 0 then
    []
  else
    let (fname, s_env, d_env, body) = List.hd pending
    in
      if List.mem fname processed then
        specialize_func (List.tl pending) prog div processed
      else (
        let (reduced, new_pending) = reduce body (s_env @ d_env) prog div
        in (fname, (get_names d_env, reduced))::specialize_func (List.tl pending @ new_pending) prog div (fname::processed)
      )
;;

let specialize (prog:program) (div:division): program = 
  let main_func = List.assoc "main" prog
  in specialize_func [("main", [], List.map (fun x-> (x,None)) (fst main_func), snd main_func)] prog div []
;;
