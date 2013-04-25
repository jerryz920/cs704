module S = Scheme0 ;;
exception BadArgs of string;;
let docstring = "Specialize a Scheme0 program.

'Specialize.byte foo.sch foo.div' specializes the Scheme0 program 'foo.sch',
using the division given in foo.div"
;;
let program_filename = ref "" ;;
let division_filename = ref "" ;;
let count = ref 0 ;;
let arg_spec = [
  ( "-h",
    Arg.Unit (fun () -> raise (Arg.Help "Try '-help' instead.\n")),
    "\tUse -help instead.")
] ;;

let handle_anon (s:string) = 
  ( match !count with
    | 0 -> program_filename := s 
    | 1 -> division_filename := s 
    | _ -> raise (BadArgs "Too many arguments.") )
  ;
  count := !count + 1 ;;

Arg.parse arg_spec handle_anon docstring ;;

if !count < 1 then raise 
  (BadArgs "Not enough arguments: I need a .sch and a .div file.") ;;

let prog = try S.program_from_file !program_filename
  with 
    | S.SyntaxError s ->
      Format.print_string ("Scheme0 syntax error: "^s^"\n"); exit 1 
    | Sys_error s ->
      Format.print_string s; Format.print_newline (); exit 1
in
let div =
  try S.division_from_file !division_filename
  with 
    | S.SyntaxError s ->
      Format.print_string ("Division syntax error: "^s^"\n"); exit 1 
    | Sys_error s ->
      Format.print_string s; Format.print_newline (); exit 1
in
let output =
  try S.string_of_program (S.specialize prog div)
  with
    | S.UtilityError -> "Error in Scheme0Utils. Contact Matt. <elder@cs>\n"
    | S.EvalError s -> "Scheme0 specialization error: "^s^"\n"
in
Format.print_string output;;

