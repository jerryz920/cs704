open Lambda ;;

type env_entry = string * gentyp  (*env_entry is a pair of string (identifier) and gentyp*)
type env = env_entry list  (*the environment A is represented by a list whose content is env_entry*)

let init_typenv = [("+",([],Fun(Int,Fun(Int,Int)))); (*Int->Int->Int*)
	    ("-",([],Fun(Int,Fun(Int,Int))));  (*Int->Int->Int*)
	    ("*",([],Fun(Int,Fun(Int,Int))));  (*Int->Int->Int*)
	    ("div",([],Fun(Int,Fun(Int,Int))));  (*Int->Int->Int*)
            ("mod",([],Fun(Int,Fun(Int,Int))));  (*Int->Int->Int*)
	    ("<",([],Fun(Int,Fun(Int,Bool))));  (*Int->Int->Bool*)
	    (">",([],Fun(Int,Fun(Int,Bool))));  (*Int->Int->Bool*)
	    ("==",(["a"],Fun(Tvar("a"),Fun(Tvar("a"),Bool)))); (*Int->Int->Bool*) (*changed this in debug process, it should be a->a-> bool*)
            ("cons",(["a"],Fun(Tvar("a"),Fun(List(Tvar("a")),List(Tvar("a")) ))));  (*forall a. a -> a-list ->a-list*)
            ("car",(["a"],Fun(List(Tvar("a")),Tvar("a"))));  (*forall a. a-list -> a*)
	    ("cdr",(["a"],Fun(List(Tvar("a")),Tvar("a"))));  (*forall a. a-list -> a*)
	    ("isnil",(["a"],Fun(List(Tvar("a")),Bool))) (*forall a. a-list -> Bool*)
	    ]
            ;;
let add_entry (newe:env_entry) (typenv:env) : env=
	    let key = fst(newe) in
	    [newe] @ (List.remove_assoc key typenv) (*remove the old key's value and update the new value associated with it, if old exists; otherwise, just act as adding a new key value pair*)
	    ;; 

let find_value_of_id (key:string) (typenv:env) : gentyp=
	     List.assoc key typenv;;

let print_stringlist l =
        List.iter print_string l
        ;;
(*let print_entry (e:env_entry ) =
        print_string (fst(e));
        print_string ";";
        print_stringlist (fst(snd(e)));
        print_typ (snd(snd(e)));
	;;
let print_env (e:env) = 
	List.iter print_entry e;
	;;
*)
