open Syntax

let rec subst (expr : e_t) (var : string) (value : v_t) : e_t =
  match expr with
  | Value (Lam (x, e)) ->
    if x = var then expr
    else Value (Lam (x, subst e var value))
  | Value (_) -> expr
  | Var (x) -> if x = var then Value (value) else expr
  | App (e1, e2) -> App (subst e1 var value, subst e2 var value)
  | Plus (e1, e2) -> Plus (subst e1 var value, subst e2 var value)
  | Reset (e) -> Reset (subst e var value)
  | Shift (k, e) ->
    if k = var then expr
    else Shift (k, subst e  var value)

let var_names = ref []
let var_name_examples =
  let rec a_to_w i =
    if i > 119 then [] else Char.escaped (Char.chr i) :: a_to_w (i + 1) in
  let rec x_to_z i =
    if i > 122 then a_to_w 97 else Char.escaped (Char.chr i) :: x_to_z (i + 1) in
  x_to_z 120
let add_var_name (var : string) : unit =
  let current_var_names = !var_names in
  if List.mem var current_var_names
  then ()
  else var_names := var :: current_var_names
let rec record_var_name (expr : e_t) : unit = match expr with
  | Value (Lam (x, e)) -> add_var_name x; record_var_name e
  | Value (_) -> ()
  | Var (_) -> ()
  | App (e1, e2) -> record_var_name e1; record_var_name e2
  | Plus (e1, e2) -> record_var_name e1; record_var_name e2
  | Reset (e) -> record_var_name e
  | Shift (k, e) -> add_var_name k; record_var_name e
let gen_var_name () =
  let new_var =
    try List.find (fun var -> not (List.mem var !var_names)) var_name_examples
    with Not_found -> "x" ^ string_of_int (List.length !var_names) in
  var_names := new_var :: !var_names;
  new_var
