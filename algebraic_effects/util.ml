open Syntax

(* subst e x v で e[v/x] を返す *)
let rec subst (com : c_t) (var : string) (value : v_t) : c_t =
  match com with
  | Return (v) -> Return (subst_value v var value)
  | Op (name, v, y, c) ->
    Op (name, v, y, if y = var then c else subst c var value)
  | Do (x, c1, c2) ->
    Do (x, subst c1 var value, if x = var then c2 else subst c2 var value)
  | If (v, c1, c2) ->
    If (subst_value v var value, subst c1 var value, subst c2 var value)
  | App (v1, v2) ->
    App (subst_value v1 var value, subst_value v2 var value)
  | With (v, c) ->
    With (subst_value v var value, subst c var value)

and subst_handler (handler : h_t) (var : string) (value : v_t) : h_t =
  match handler with (return_opt, op_lst) ->
    let return = match return_opt with
      | None -> None
      | Some (x, c) -> Some (x, if x = var then c else subst c var value) in
    let ops =
      List.map
        (fun (name, x, k, c) ->
           (name, x, k, if x = var || k = var then c else subst c var value))
        op_lst in
    (return, ops)

and subst_value (v : v_t) (var : string) (value : v_t) : v_t = match v with
  | Var (x) -> if x = var then value else v
  | Fun (x, c) -> if x = var then v else Fun (x, subst c var value)
  | Handler (h) -> Handler (subst_handler h var value)
  | _ -> v

let rec subst2 : c_t -> (string * v_t * string * v_t) -> c_t =
  fun com ((var1, val1, var2, val2) as targets) -> match com with
    | Return (v) -> Return (subst2_value v targets)
    | Op (name, v, y, c) ->
      Op (name, v, y, if y = var1 || y = var2 then c else subst2 c targets)    
    | Do (x, c1, c2) ->
      Do (x, subst2 c1 targets,
          if x = var1 || x = var2 then c2 else subst2 c2 targets)
    | If (v, c1, c2) ->
      If (subst2_value v targets, subst2 c1 targets, subst2 c2 targets)
    | App (v1, v2) ->
      App (subst2_value v1 targets, subst2_value v2 targets)
    | With (v, c) ->
      With (subst2_value v targets, subst2 c targets)

and subst2_handler : h_t -> (string * v_t * string * v_t) -> h_t =
  fun (return_opt, op_lst) ((var1, val1, var2, val2) as targets) ->
    let return = match return_opt with
      | None -> None
      | Some (x, c) ->
        Some (x, if x = var1 || x = var2 then c else subst2 c targets) in
    let ops =
      List.map
        (fun (name, x, k, c) ->
           (name, x, k, if x = var1 || x = var2 then c else subst2 c targets))
        op_lst in
    (return, ops)

and subst2_value : v_t -> (string * v_t * string * v_t) -> v_t =
  fun value ((var1, val1, var2, val2) as targets) -> match value with
    | Var (x) -> if x = var1 then val1 else if x = var2 then val2 else value
    | Fun (x, c) ->
      if x = var1 || x = var2 then value else Fun (x, subst2 c targets)
    | Handler (h) -> Handler (subst2_handler h targets)
    | _ -> value

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
  | Return (Fun (x, c)) -> add_var_name x; record_var_name c
  | Return (Handler (h)) -> record_var_name_handler h
  | Return (v) -> record_var_name_value v
  | Op (op, v, y, c) ->
    record_var_name_value v;
    add_var_name y; record_var_name c
  | Do (x, c1, c2) -> add_var_name x; record_var_name c1; record_var_name c2
  | If (v, c1, c2) ->
    record_var_name_value v; record_var_name c1; record_var_name c2
  | App (v1, v2) -> record_var_name_value v1; record_var_name_value v2
  | With (v, c) -> record_var_name_value v; record_var_name c

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
