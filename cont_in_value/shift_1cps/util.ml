open Syntax

let used_names : string list ref = ref []

let name_examples : string list =
  let rec a_to_w i =
    if i > 119 then [] else Char.escaped (Char.chr i) :: a_to_w (i + 1) in
  let rec x_to_z i =
    if i > 122 then a_to_w 97
    else Char.escaped (Char.chr i) :: x_to_z (i + 1) in
  x_to_z 120

let add (name : string) : unit =
  let old_list = !used_names in
  if List.mem name old_list
  then ()
  else used_names := name :: old_list

let rec collect : t -> unit = function
  | Var (_) -> ()
  | Fun (x, t) -> add x; collect t
  | App (t1, t2) -> collect t1; collect t2
  | Shift (x, t) -> add x; collect t;
  | Reset (t) -> collect t

let gen_var_name () : string =
  let new_name =
    try List.find (fun var -> not (List.mem var !used_names)) name_examples
    with Not_found -> "x" ^ string_of_int (List.length !used_names) in
  used_names := new_name :: !used_names;
  new_name
