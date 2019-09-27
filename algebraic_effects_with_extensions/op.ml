open Syntax

type op_result_t = (v_t * string option * string option)
type mode_t = CUI
            | Test

let mode : mode_t ref = ref CUI
let channel : in_channel ref = ref stdin

let read_line () : string =
  match !mode with
  | CUI -> input_line stdin
  | Test ->
    let line = input_line !channel in
    print_endline line;
    line

exception Raise of string

let do_op (op : op_t) (arg : v_t) : op_result_t = match (op, arg) with
  | (Read, Unit) ->
    print_string "new input> ";
    flush stdout;
    let read = read_line () in
    print_newline ();
    (String (read), Some (read), None)
  | (Print, String (s)) ->
    (Unit, None, Some (s))
  | (Raise, String (s)) ->
    raise (Raise s)
  | (Decide, Unit) -> failwith ("no behavior defined for decide")
  | _ -> failwith ("type error in " ^ op_to_string op)
