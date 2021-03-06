open Syntax

let rec subst_v (v : v) (pairs : (string * v) list) : v = match v with
  | Var (x) -> (try List.assoc x pairs with Not_found -> v)
  | Fun (x, e) -> if List.mem_assoc x pairs then v else Fun (x, subst e pairs)
  | Cont (cont_in, x, ctxt_in) ->
    if List.mem_assoc x pairs then v
    else Cont (cont_in, x, subst_ctxt_in ctxt_in pairs)

and subst (e : e) (pairs : (string * v) list) : e = match e with
  | Val (v) -> Val (subst_v v pairs)
  | App (e1, e2) -> App (subst e1 pairs, subst e2 pairs)
  | Shift (x, e) ->
    if List.mem_assoc x pairs then e else Shift (x, subst e pairs)
  | Reset (e) -> Reset (subst e pairs)

and subst_frame (frame : frame) (pairs : (string * v) list) : frame =
  match frame with
  | CApp2 (e1) -> CApp2 (subst e1 pairs)
  | CApp1 (v2) -> CApp1 (subst_v v2 pairs)

and subst_ctxt_in (ctxt_in : ctxt_in) (pairs : (string * v) list) : ctxt_in
  = List.map (fun frame -> subst_frame frame pairs) ctxt_in

(* プログラム内で使われている変数のリストを格納する変数 *)
let var_names = ref []

(* 新しい変数を作成する時の名前の例のリスト *)
let var_name_examples =
  let rec a_to_w i =
    if i > 119 then [] else Char.escaped (Char.chr i) :: a_to_w (i + 1) in
  let rec x_to_z i =
    if i > 122 then a_to_w 97
    else Char.escaped (Char.chr i) :: x_to_z (i + 1) in
  x_to_z 120

(* 使われている変数リストに新しい変数名を追加する *)
let add_var_name (var : string) : unit =
  let current_var_names = !var_names in
  if List.mem var current_var_names
  then ()
  else var_names := var :: current_var_names

let rec record_var_name : e -> unit = function
  | Val (v) -> record_var_name_value v
  | App (e1, e2) -> record_var_name e1; record_var_name e2
  | Shift (x, e) -> add_var_name x; record_var_name e
  | Reset (e) -> record_var_name e

and record_var_name_value : v -> unit = function
  | Var (x) -> ()
  | Fun (x, e) -> add_var_name x; record_var_name e
  | Cont (cont_in, x, ctxt_in) -> add_var_name x

(* プログラム内でまだ使われていない変数名を生成して返す *)
let gen_var_name () =
  let new_var =
    try List.find (fun var -> not (List.mem var !var_names)) var_name_examples
    with Not_found -> "x" ^ string_of_int (List.length !var_names) in
  var_names := new_var :: !var_names;
  new_var

