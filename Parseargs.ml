open Format ;;

(* Parseargs.ml: simple argument parsing.*)

exception BadArgs of string ;;

(* parse_args n msg: Return a list of n arguments, or fail and print msg.
   
   
*)
let parse_args numnames docstring = 
  let filename = ref [] in
  let count = ref 0 in
  let arg_spec = [
    ( "-h",
      Arg.Unit (fun () -> raise (Arg.Help "Try '-help' instead.\n")),
      "\tUse -help instead.")
  ] in

  let handle_anon (s:string) = 
    incr count;
    if !count > numnames then raise (BadArgs "Too many arguments.")
    else filename := s :: !filename in
    
  Arg.parse arg_spec handle_anon docstring;

  if !count < numnames
  then raise (BadArgs "Too few arguments.");
  
  List.rev !filename ;;
