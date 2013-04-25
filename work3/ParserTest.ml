module S = Scheme0;;

exception BadArgs of string;;

(* Parse arguments *)
let docstring = "Test the Scheme0 parsers." ;;
let program_filename = ref "" ;;
let count = ref 0 ;;
let arg_spec = [
  ( "-h",
    Arg.Unit (fun () -> raise (Arg.Help "Try '-help' instead.\n")),
    "\tUse -help instead.")
] ;;

let handle_anon (s:string) = 
  count := !count + 1;
  if !count > 1 then raise (BadArgs "Too many arguments.")
  else program_filename := s ;;

let suffix s =
  let i = String.rindex s '.' in
  String.sub s (i+1) ((String.length s) - i - 1) ;;
  
let echo reader writer = 
  let input = try reader !program_filename
    with 
      | S.SyntaxError s ->
        Format.print_string ("Syntax error: "^s^"\n");
        exit 1 
      | Sys_error s ->
        Format.print_string s; Format.print_newline ();
        exit 1
  in
  let output =
    try writer input
    with
      | S.UtilityError -> "Error in Scheme0Utils. Contact Matt. <elder@cs>"
      | S.EvalError s -> "Scheme0 error: "^s
  in
  Format.print_string output;
  Format.print_newline () ;;

Arg.parse arg_spec handle_anon docstring;;

match suffix !program_filename with
  | "sch" -> echo S.program_from_file S.string_of_program
  | "div" -> echo S.division_from_file S.string_of_division
  | "asch" -> echo S.a_program_from_file S.string_of_a_program
  | s -> raise (BadArgs
   ("Invalid suffix "^s^". need '.sch', '.div', or '.asch' files.") );;
