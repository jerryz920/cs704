include Scheme0Utils
(*implement union *)
let union (b1:bind_time) (b2:bind_time):bind_time =
	if b1 == S && b2 == S then S else D
	;;
(*implement B_e *)
let rec bind_expr (e:expr) (tao:bind_time_env):bind_time =
	let list_of_formal_bind = fst(tao) in (* (string * bind_time) list*)
	match e with 
	|Val(v) -> S
	|Var(v) -> List.assoc v list_of_formal_bind
	|If(e1,e2,e3) -> let r1 =  bind_expr e1 tao in
			 let r2 = bind_expr e2 tao in
			 let r3 = bind_expr e3 tao in
			 union r1 (union r2 r3)
	|Call(s,l) -> let l1 = List.map (fun x -> bind_expr x tao) l in (*bind_expr e_i tao, where e_i is the ith element of list l (recall that l is a list of expr).*)
		      let r = List.fold_left (union) S l1 in   (* union each element of list l1; if any one of the elements is D, then r is D, else r is S*)
	   	      r
	|Binop(bin, e1, e2) -> union (bind_expr e1 tao) (bind_expr e2 tao)
	|Unop(un, e) -> (bind_expr e tao)
	;;

(*test case: 1.
	     let tao = ([("a",D);("b",S)],D);;
	     let e = Binop(Plus, Var("b"), Var("a"));;
	     bind_expr e tao;;
  	     2.
	     let tao = ([("a",D);("b",S)],D);;
             let e = Binop(Plus, Var("b"), If(Val(Boolean(true)),Var("a"),Var("b")));;
	     bind_expr e tao;;
	     3.
	     let tao = ([("a",D);("b",S)],D);;
             let e = Binop(Plus, Var("b"), If(Val(Boolean(false)),Var("a"),Var("b")));;
             bind_expr e tao;;
	     4.
	     let tao = ([("a",D);("b",S)],D);;
             let e = Binop(Plus, Var("b"), If(Val(Boolean(false)),Var("b"),Var("b")));;
             bind_expr e tao;;
*)
(* Implement these. *)
let bind_time_analysis (prog:program) :division = []

let specialize (prog:program) (div:division): program = []
