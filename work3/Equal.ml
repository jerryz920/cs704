module S = Scheme0 ;;
exception BadArgs of string;;

let docstring = "Compare two programs or divisions.

This gets called by the `check` and `grade` scripts. "
;;

type checker_mode = Program | Division ;;

let assoc_equal a b =
  let rec els_equal a b =
    match a with 
      | [] -> true
      | (key, v) :: tail ->
        (List.mem_assoc key b)
        && (List.assoc key b) = v
        && els_equal tail b 
  in
  (List.length a == List.length b) && (els_equal a b) ;;

let filename_1 = ref "" ;;
let filename_2 = ref "" ;;
let count = ref 0 ;;
let mode = ref Program ;;
let arg_spec = [
  ( "-h",
    Arg.Unit (fun () -> raise (Arg.Help "Try '-help' instead.\n")),
    "\tUse -help instead.");
  ( "-div",
    Arg.Unit (fun () -> mode := Division),
    "\tCheck divisions for equality.");
  ( "-sch",
    Arg.Unit (fun () -> mode := Program),
    "\tCheck Scheme0 programs for equality.");
] ;;

let handle_anon (s:string) = 
  (match !count with
    | 0 -> filename_1 := s 
    | 1 -> filename_2 := s 
    | _ -> raise (BadArgs "Too many arguments."));
  count := !count + 1 ;;

Arg.parse arg_spec handle_anon docstring ;;

if !count < 2 then raise (BadArgs "Need an argument: name two files.") ;;

let is_equal = match !mode with 
  | Division -> assoc_equal
    (S.division_from_file !filename_1)
    (S.division_from_file !filename_2)
  | Program -> assoc_equal
    (S.program_from_file !filename_1)
    (S.program_from_file !filename_2)
in
if is_equal then (exit 0) else (exit 1)
