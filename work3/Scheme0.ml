include Scheme0Utils;;
open Format;;
(*implement union *)
let union (b1:bind_time) (b2:bind_time):bind_time =
	if b1 == S && b2 == S then S else D
	;;
(*implement B_e *)
type btenv = (string * bind_time) list;;
let rec bind_expr (e:expr) (tao:btenv):bind_time =
	match e with 
	|Val(v) -> S
	|Var(v) -> List.assoc v tao
	|If(e1,e2,e3) -> let r1 =  bind_expr e1 tao in
			 let r2 = bind_expr e2 tao in
			 let r3 = bind_expr e3 tao in
			 union r1 (union r2 r3)
	|Call(s,l) -> let l1 = List.map (fun x -> bind_expr x tao) l in (*bind_expr e_i tao, where e_i is the ith element of list l (recall that l is a list of expr).l1 is a list of bind_time*)
		      let r = List.fold_left (union) S l1 in   (* union each element of list l1; if any one of the elements is D, then r is D, else r is S*)
	   	      r
	|Binop(bin, e1, e2) -> union (bind_expr e1 tao) (bind_expr e2 tao)
	|Unop(un, e1) -> (bind_expr e1 tao)
	;;

(*test case: 1.
	     let tao = ([("a",D);("b",S)]);;
	     let e = Binop(Plus, Var("b"), Var("a"));;
	     bind_expr e tao;;
  	     2.
	     let tao = ([("a",D);("b",S)]);;
             let e = Binop(Plus, Var("b"), If(Val(Boolean(true)),Var("a"),Var("b")));;
	     bind_expr e tao;;
	     3.
	     let tao = ([("a",D);("b",S)]);;
             let e = Binop(Plus, Var("b"), If(Val(Boolean(false)),Var("a"),Var("b")));;
             bind_expr e tao;;
	     4.
	     let tao = ([("a",D);("b",S)]);;
             let e = Binop(Plus, Var("b"), If(Val(Boolean(false)),Var("b"),Var("b")));;
             bind_expr e tao;;
	     5.
	     let tao = ([("a",D);("b",S)]);;
             let e = Unop(Car, Var("b"));;
             bind_expr e tao;;
	     6.
	     let tao = ([("a",D);("b",S)]);;
             let l = [Unop(Car, Var("b")); Binop(Plus, Var("b"), If(Val(Boolean(true)),Var("a"),Var("b")))];;
             let e = Call(f,l);;
             bind_expr e tao;;
*)
let list_union (bt1:btenv) (bt2:btenv):btenv =
	(*print_string "hello\n";*)
	List.map2 (fun x y -> (fst(x),union (snd(x)) (snd(y)) ) ) bt1 bt2
	;;
(*let bt1 = [("a",S);("b",D)];;
  let bt2 = [("a",D);("b",S)];;
  list_union bt1 bt2;;
*)
let rec bind_expr_fn (e:expr) (tao:btenv) (g:string) (gtao:btenv): btenv = (*tao is function f_i's tao and e is function f_i's expression*)
	(*the return is function g's update tao, tao is type of btenv*)
	match e with
	|Val(v) -> List.map (fun x -> (fst(x),S)) gtao 
	|Var(v) -> List.map (fun x -> (fst(x),S)) gtao
	|If(e1,e2,e3) -> let r1 = bind_expr_fn e1 tao g gtao in
			 let r2 = bind_expr_fn e2 tao g gtao in
			 let r3 = bind_expr_fn e3 tao g gtao in
			 
			 list_union r1 (list_union r2 r3) 
	(**|Call(f,l) -> List.map (fun x -> (fst(x),D)) gtao *)
	
	|Call(f,l) ->  let l1 = List.map (fun x -> bind_expr_fn x tao g gtao) l in (*bind_expr_fn e_i tao g, where e_i is the ith element of list l (recall that l is a list of expr).Note that l1 is a list of btenv*)
		       (*the btenv in l1 should be the same type as gtao*)
		       
		       let list_static = List.map (fun x -> (fst(x),S)) gtao in
		       let t = List.fold_left (list_union) list_static l1 in (*the element of l1 is also a list:btenv*)
		       if not(f=g) then t (*short circurt*) 
	
		       else 
		       ( let list_after_call_bind_expr = List.map2 (fun x y -> (fst(x), bind_expr y tao)) gtao l in (*the order of tao and l should be same*)
			 list_union t list_after_call_bind_expr
		       )
	
	|Binop(bin, e1, e2) -> list_union (bind_expr_fn e1 tao g gtao) (bind_expr_fn e2 tao g gtao)
	
	|Unop(un, e1) -> (bind_expr_fn e1 tao g gtao)
	
	;;

(*test case: 1.
	     let g = "g";;
             let tao = ([("a",S);("b",S)]);;
             let e = Binop(Plus, Var("b"), Var("a"));;
	     bind_expr_fn e tao g;;
	     2.
	     let g = "g";;
             let tao = ([("a",S);("b",S)]);;
	     let e = Binop(Plus, Var("b"), If(Val(Boolean(false)),Var("a"),Var("b")));;
             bind_expr_fn e tao g;;
	     3.
             let g = "g";;
             let tao = ([("a",S);("b",D)]);;
	     let f ="f";;
	     let l = [Unop(Car, Var("b")); Binop(Plus, Var("b"), If(Val(Boolean(true)),Var("a"),Var("b")))];;
             let e = Call(g,l);;
             bind_expr_fn e tao g;;
	     4.
	     let g = "g";;
             let tao = ([("a",S);("b",D)]);;
             let f ="f";;
             let l = [Unop(Car, Var("b")); Binop(Plus, Var("b"), If(Val(Boolean(true)),Var("a"),Var("b")))];;
             let e = Call(f,l);;
             bind_expr_fn e tao g;;
	     5.
	     let g = "g";;
             let tao = ([("a",S);("b",D)]);;
             let f ="f";;
             let l = [Unop(Car, Var("b"))];;
             let e = Call(f,l);;
             bind_expr_fn e tao g;;

*)
type div = (string * btenv) list;; (*Note that div != division, div is an intermediate result*)
type fn_bte = (string * btenv);;
type btenv_list = btenv list;;
type fn_expr = (string * expr) list;; (*function name and expr : list*)

let compare (d1:div) (d2:div) : bool = (*if d1 and d2 are the same return true else flase*)
	let names = List.map fst d1 in
	let equal_each (name:string) : bool = (List.assoc name d1) = (List.assoc name d2) 
	in
	let all_bools = List.map equal_each names in
	List.fold_left (&&) true all_bools
	;;
(*test case: 1.
	     let d1 = [("main",[("a",D);("b",S)]); ("function1",[("c",S);("d",S)])];;
	     let d2 = [("function1",[("c",S);("d",S)]); ("main",[("a",D);("b",S)])];;
	     compare d1 d2;;
	     2.
	     let d1 = [("main",[("a",D);("b",S)]); ("function1",[("c",S);("d",S)])];;
             let d2 = [("function1",[("c",S);("d",D)]); ("main",[("a",D);("b",S)])];;
*)	

(*find least dynamic div*)
(*one import thing is that the order of the functions in div might be different for different divs; but given a certian function f, then in f's btenv, the formals order is always the same*)
let rec find_least (d:div) (fe:fn_expr) :div = 
	(*this function corresponds to bind_expr_fn e_i (div f_i) g where i is the ith function*)
	let func_i_impacts (fb:fn_bte) (g:string) : btenv = 
	(*fb is the basic element of list d*) 
		let name = fst(fb) in (*name of the function, i.e., f_i*)
		let gtao = List.assoc g d in  (*this is function g 's tao, NOT f_i 's tao*)
	   	bind_expr_fn (List.assoc name fe) (List.assoc name d) g gtao(*List.assoc name fe==expr; List.assoc name d==btenv*)
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
(*test case: 1.
	     let d = [("main",[("a",D);("b",D)]); ("function1",[("c",S);("d",S)])];;
	     let g = "function1";;
	     let l = [Unop(Car, Var("b")); Binop(Plus, Var("b"), If(Val(Boolean(true)),Var("a"),Var("b")))];;   (*the first formal is S, the second is D*)
	     let fe = [("main",Call(g,l)); ("function1", Unop(Car, Var("c")) )];;
	     let result = find_least d fe;;
	     bind_expr (List.assoc "function1" fe) (List.assoc "function1" result);;
	     2.
	     let d = [("main",[("a",D);("b",D)]); ("function1",[("c",S);("d",S)])];;
             let g = "function1";;
             let l = [Unop(Car, Var("b")); Binop(Plus, Var("b"), If(Val(Boolean(true)),Var("a"),Var("b")))];;   (*the first formal is S, the second is D*)
             let fe = [("main",Call(g,l)); ("function1", Binop(Plus, Var("c"), If(Val(Boolean(true)),Var("c"),Var("d")))) ];;
             let result = find_least d fe;;
             bind_expr (List.assoc "function1" fe) (List.assoc "function1" result);;
	     3.
	     let d = [("main",[("a",D);("b",D)]); ("function1",[("c",S);("d",S)])];;
             let fe = [("main",Unop(Car, Var("b")))]; ("function1", Binop(Plus, Var("c"), If(Val(Boolean(true)),Var("c"),Var("d")))) ];;
             let result = find_least d fe;;
             bind_expr (List.assoc "function1" fe) (List.assoc "function1" result);;
	     4.
	     let d = [("main",[("a",D);("b",D)]); ("function1",[("c",S);("d",S)])];;
             let fe = [("main", Unop (Car, Var "b")); ("function1", Unop (Car, Var "c"))];;
	     let result = find_least d fe;;
             bind_expr (List.assoc "function1" fe) (List.assoc "function1" result);;
	     5.
	     let d = [("main",[("a",D);("b",D)])];;
	     let fe = [("main", Unop (Car, Var "b"))];;
	     let result = find_least d fe;;
	     bind_expr (List.assoc "main" fe) (List.assoc "main" result);;
	     6.
	     let d = [("main",[("a",D);("b",S)]); ("function1",[("c",S);("d",S)]); ("function2",[("e",S);("f",S)])];;
             let g1 = "function1";;
	     let g2 = "function2";;
             let l1 = [Unop(Car, Var("b")); Binop(Plus, Var("b"), If(Val(Boolean(true)),Var("a"),Var("b")))];;   (*the first formal is S, the second is D*)
	     let l2 = [Unop (Car, Var "c") ; Unop (Car, Var "d")];;
             let fe = [("main",Call(g1,l1)); ("function1", Call(g2,l2)); ("function2", Unop (Car, Var "e")) ];;
             let result = find_least d fe;;
             bind_expr (List.assoc "function2" fe) (List.assoc "function2" result);;
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
	
(*test case: 1.
	     let prog = [("main",(["a";"b"], Unop(Car, Var("b")))) ; ("f1",(["c";"d"], Unop(Car, Var("d")))) ; ("f2",(["e";"f"], Unop(Car, Var("e")))) ];;
	     init_div prog;;
	     2.
	     let prog = [("main",(["a";"b"], Unop(Car, Var("b"))))];;
	     init_div prog;;
*)	
		
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
			let expr_type = bind_expr (List.assoc name name_expr) bt
			in
			if has_dynamic then (bt,D) else
			(bt,expr_type)
			)
		in
		List.map (fun x -> (fst(x), get_bind_time_env x ismain0_value)) d
	in
	get_divison final_div fe
	;;
(*test case: 1.
	     let prog = [("main",(["a";"b"], Unop(Car, Var("b"))))];;
	     bind_time_analysis prog;;
	     2.
	     let prog = [("main",(["a";"b"], Unop(Car, Var("b")))) ; ("f1",(["c";"d"], Unop(Car, Var("d")))) ; ("f2",(["e";"f"], Unop(Car, Var("e")))) ];;
	     bind_time_analysis prog;;
	     3.
	     let prog = [("main",(["a";"b"], Call("f1", [Unop(Car, Var("b")); Val(Number(1))] ))) ; ("f1",(["c";"d"], Unop(Car, Var("d")))) ; ("f2",(["e";"f"], Unop(Car, Var("e")))) ];;
	     bind_time_analysis prog;;
	     4.
	     let prog = [("main",(["a";"b"], Call("f1", [Unop(Car, Var("b")); Val(Number(1))] ))) ; ("f1",(["c";"d"], Unop(Car, Var("c")))) ; ("f2",(["e";"f"], Unop(Car, Var("e")))) ];;
             bind_time_analysis prog;;
	     5.
	     let prog = [("main",(["a";"b"], Call("f1", [Unop(Car, Var("b")); Val(Number(1))] ))) ; ("f1",(["c";"d"], Call("f2", [Unop(Car, Var("d")); Unop(Car, Var("c"))] ))) ; ("f2",(["e";"f"], Unop(Car, Var("e")))) ];;
             bind_time_analysis prog;;
*)	
let specialize (prog:program) (div:division): program = []
