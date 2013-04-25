module S = Scheme0 ;;
exception BadArgs of string;;
let docstring = "Perform binding time analysis on a Scheme0 program.

'BindTimeAnalysis.byte foo.sch' does binding time analysis on 'foo.sch'."
;;
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

Arg.parse arg_spec handle_anon docstring ;;

if !count = 0 then raise (BadArgs "Need an argument: name a .sch file.") ;;

let input = try S.program_from_file !program_filename
  with 
    | S.SyntaxError s ->
      Format.print_string ("Scheme0 syntax error: "^s^"\n"); exit 1 
    | Sys_error s ->
      Format.print_string s; Format.print_newline (); exit 1
in
let output =
  try S.string_of_division (S.bind_time_analysis input)
  with
    | S.UtilityError -> "Error in Scheme0Utils. Contact Matt. <elder@cs>\n"
    | S.EvalError s -> "Scheme0 binding-time analysis error: "^s^"\n"
in
Format.print_string output ;;

