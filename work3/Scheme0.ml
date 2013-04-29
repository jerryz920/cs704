include Scheme0Utils
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
	List.map2 (fun x y -> (fst(x),union (snd(x)) (snd(y)) ) ) bt1 bt2
	;;
(*let bt1 = [("a",S);("b",D)];;
  let bt2 = [("a",D);("b",S)];;
  list_union bt1 bt2;;
*)
let rec bind_expr_fn (e:expr) (tao:btenv) (g:string) : btenv = (*tao is function g's tao*)
	match e with
	|Val(v) -> List.map (fun x -> (fst(x),S)) tao 
	|Var(v) -> List.map (fun x -> (fst(x),S)) tao
	|If(e1,e2,e3) -> let r1 = bind_expr_fn e1 tao g in
			 let r2 = bind_expr_fn e2 tao g in
			 let r3 = bind_expr_fn e3 tao g in
			 list_union r1 (list_union r2 r3)
	|Call(f,l) ->  let l1 = List.map (fun x -> bind_expr_fn x tao g) l in (*bind_expr_fn e_i tao g, where e_i is the ith element of list l (recall that l is a list of expr).Note that l1 is a list of btenv*)
		       let list_static = List.map (fun x -> (fst(x),S)) tao in
		       let t = List.fold_left (list_union) list_static l1 in (*the element of l1 is also a list:btenv*)
		       if not(f=g) then t (*short circurt*)
		       else 
		       ( let list_after_call_bind_expr = List.map2 (fun x y -> (fst(x), bind_expr y tao)) tao l in (*the order of tao and l should be same*)
			 list_union t list_after_call_bind_expr
		       )
	|Binop(bin, e1, e2) -> list_union (bind_expr_fn e1 tao g) (bind_expr_fn e2 tao g)
	|Unop(un, e1) -> (bind_expr_fn e1 tao g)
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
	
let rec find_least (d:div) (fe:fn_expr) :div = (*find least dynamic div*)
	let func_i_impacts (fb:fn_bte) (g:string) : btenv = 
							    let name = fst(fb) in (*name of the function*)
						            bind_expr_fn (List.assoc name fe) (List.assoc name d) g(*List.assoc name fe==expr; List.assoc name d==btenv*)
	in
	let func_all_impacts (g:string) : btenv_list = List.map (fun x -> func_i_impacts x g) d (*x is type of fn_bte; d is a list of x(d is a list of fn_bte)*)
	in let g_tao (g:string) :btenv = List.assoc g d in
	let list_static (g:string) :btenv = List.map (fun x -> (fst(x),S)) (g_tao g) in
	
	let union_func_all_impacts (g:string) :btenv = List.fold_left (list_union)  (list_static g) (func_all_impacts g) in
	let get_out_of_main = List.filter (fun x -> not(x="main")) d in (*get_out_of_main is type of div, but it does not include main's information*)
	let get_out_of_main_names = List.map fst get_out_of_main in (*only function names, a string list*)
	let newdiv_wo_main = List.map union_func_all_impacts get_out_of_main_names in
	let main_info = [("main",(List.assoc "main" d))] in
	let newdiv = List.append main_info newdiv_wo_main in
	if compare newdiv d then d else (find_least newdiv fe)
	;;
	
	
(* Implement these. *)
let bind_time_analysis (prog:program) :division = []

let specialize (prog:program) (div:division): program = []
