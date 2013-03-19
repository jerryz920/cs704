open Lambda ;;

let typenv = [("+",([],Fun(Int,Fun(Int,Int)))); (*Int->Int->Int*)
	    ("-",([],Fun(Int,Fun(Int,Int))));  (*Int->Int->Int*)
	    ("*",([],Fun(Int,Fun(Int,Int))));  (*Int->Int->Int*)
	    ("div",([],Fun(Int,Fun(Int,Int))));  (*Int->Int->Int*)
            ("mod",([],Fun(Int,Fun(Int,Int))));  (*Int->Int->Int*)
	    ("<",([],Fun(Int,Fun(Int,Bool))));  (*Int->Int->Bool*)
	    (">",([],Fun(Int,Fun(Int,Bool))));  (*Int->Int->Bool*)
	    ("==",([],Fun(Int,Fun(Int,Bool)))); (*Int->Int->Bool*)
            ("cons",(["a"],Fun(Tvar("a"),Fun(List(Tvar("a")),List(Tvar("a")) ))));  (*forall a. a -> a-list ->a-list*)
            ("car",(["a"],Fun(List(Tvar("a")),Tvar("a"))));  (*forall a. a-list -> a*)
	    ("cdr",(["a"],Fun(List(Tvar("a")),Tvar("a"))));  (*forall a. a-list -> a*)
	    ("isnil",(["a"],Fun(List(Tvar("a")),Bool))) (*forall a. a-list -> Bool*)
	    ]
            ;;
let add_entry newe =
	    newe @ typenv
	    ;; 

let find_value_of_id key =
	     List.assoc key typenv;;
