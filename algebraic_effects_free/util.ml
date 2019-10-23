open Syntax

(* subst e (x, v) で e[v/x] を返す *)
let rec subst (c : c_t) (pair : (string * v_t)) : c_t =
  match c with
  | Val (v) -> Val (subst_value v pair)
  | App (c1, c2) ->
    App (subst c1 pair, subst c2 pair)
  | If (c0, c1, c2) ->
    If (subst c0 pair, subst c1 pair, subst c2 pair)
  | Op (op, c0) ->
    Op (op, subst c0 pair)
  | With (c1, c2) ->
    With (subst c1 pair, subst c2 pair)

and subst_value (v : v_t) ((var, value) as pair : (string * v_t)) : v_t =
  match v with
  | Var (x) -> if x = var then value else v
  | Fun (x, c) -> if x = var then v else Fun (x, subst c pair)
  | Handler (h) -> Handler (subst_handler h pair)
  | _ -> v

and subst_handler (handler : h_t) ((var, value) as pair : (string * v_t)) : h_t
  =  match handler with (return_opt, op_lst) ->
    let return = match return_opt with
      | None -> None
      | Some (x, c) -> Some (x, if x = var then c else subst c pair) in
    let ops =
      List.map
        (fun (name, x, k, c) ->
           (name, x, k, if x = var || k = var then c else subst c pair))
        op_lst in
    (return, ops)

let rec subst_all (c : c_t) (pairs : (string * v_t) list) : c_t = match c with
  | Val (v) -> Val (subst_all_value v pairs)
  | App (c1, c2) -> App (subst_all c1 pairs, subst_all c2 pairs)
  | If (c0, c1, c2) ->
    If (subst_all c0 pairs, subst_all c1 pairs, subst_all c2 pairs)
  | Op (op, c0) -> Op (op, subst_all c0 pairs)
  | With (c1, c2) -> With (subst_all c1 pairs, subst_all c2 pairs)

and subst_all_value (v : v_t) (pairs : (string * v_t) list) : v_t
  = match v with
  | Var (x) -> begin
      try snd (List.find (fun (var, _) -> var = x) pairs)
      with Not_found -> v end
  | True | False -> v
  | Fun (x, c) ->
    if List.exists (fun (var, _) -> x = var) pairs
    then v else Fun (x, subst_all c pairs)
  | Handler (h) -> Handler (subst_all_handler h pairs)

and subst_all_handler (handler : h_t) (pairs : (string * v_t) list) : h_t =
  match handler with (return_opt, op_lst) ->
    let return = match return_opt with
      | None -> None
      | Some (x, c) ->
        Some (x, if List.exists (fun (var, _) -> var = x) pairs
              then c else subst_all c pairs) in
    let ops =
      List.map
        (fun (name, x, k, c) ->
           (name, x, k,
            if List.exists (fun (var, _) -> var = x || var = k) pairs
            then c else subst_all c pairs))
        op_lst in
    (return, ops)

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

(* 式を受け取ってその中の変数名を !var_names に追加する *)
let rec record_var_name (com : c_t) : unit = match com with
  | Val (Fun (x, c)) -> add_var_name x; record_var_name c
  | Val (Handler (h)) -> record_var_name_handler h
  | Val (v) -> record_var_name_value v
  | Op (op, c) ->
    add_var_name op;
    record_var_name c
  | If (c0, c1, c2) ->
    record_var_name c0; record_var_name c1; record_var_name c2
  | App (c1, c2) -> record_var_name c1; record_var_name c2
  | With (c0, c1) -> record_var_name c0; record_var_name c1

and record_var_name_handler ((return_opt, op_lst) : h_t) : unit =
  begin
    match return_opt with
    | None -> ()
    | Some (x, c) -> add_var_name x; record_var_name c
  end;
  List.iter
    (fun (name, x, k, c) ->
       add_var_name x; add_var_name k; record_var_name c)
    op_lst

and record_var_name_value (value : v_t) : unit = match value with
  | Fun (x, c) -> add_var_name x; record_var_name c
  | Handler (h) -> record_var_name_handler h
  | _ -> ()

(* プログラム内でまだ使われていない変数名を生成して返す *)
let gen_var_name () =
  let new_var =
    try List.find (fun var -> not (List.mem var !var_names)) var_name_examples
    with Not_found -> "x" ^ string_of_int (List.length !var_names) in
  var_names := new_var :: !var_names;
  new_var
