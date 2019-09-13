open Syntax

(* subst e x v で e[v/x] を返す *)
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

(* プログラム内で使われている変数のリストを格納する変数 *)
let var_names = ref []

(* 新しい変数を作成する時の名前の例のリスト *)
let var_name_examples =
  let rec a_to_w i =
    if i > 119 then [] else Char.escaped (Char.chr i) :: a_to_w (i + 1) in
  let rec x_to_z i =
    if i > 122 then a_to_w 97 else Char.escaped (Char.chr i) :: x_to_z (i + 1) in
  x_to_z 120

(* 使われている変数リストに新しい変数名を追加する *)
let add_var_name (var : string) : unit =
  let current_var_names = !var_names in
  if List.mem var current_var_names
  then ()
  else var_names := var :: current_var_names

(* 式を受け取ってその中の変数名を !var_names に追加する *)
let rec record_var_name (expr : e_t) : unit = match expr with
  | Value (Lam (x, e)) -> add_var_name x; record_var_name e
  | Value (_) -> ()
  | Var (_) -> ()
  | App (e1, e2) -> record_var_name e1; record_var_name e2
  | Plus (e1, e2) -> record_var_name e1; record_var_name e2
  | Reset (e) -> record_var_name e
  | Shift (k, e) -> add_var_name k; record_var_name e

(* プログラム内でまだ使われていない変数名を生成して返す *)
let gen_var_name () =
  let new_var =
    try List.find (fun var -> not (List.mem var !var_names)) var_name_examples
    with Not_found -> "x" ^ string_of_int (List.length !var_names) in
  var_names := new_var :: !var_names;
  new_var
