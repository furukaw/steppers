open Syntax

(* v の中の指定された自由変数を値に置換する *)
let rec subst_value (v : v) (pairs : (string * v) list) : v = match v with
  | Var (x) -> begin try List.assoc x pairs with Not_found -> v end
  | Fun (x, e) -> if List.mem_assoc x pairs then v else Fun (x, subst e pairs)
  | Cont (cont, (x, e)) ->
    if List.mem_assoc x pairs then v else Cont (cont, (x, subst e pairs))
  | Handler (h) -> Handler (subst_handler h pairs)

(* {return; ops} の中の指定された自由変数を値に置換する *)
and subst_handler ({return; ops} : h) (pairs : (string * v) list) : h =
  let return' = match return with
    | None -> None
    | Some (x, e) ->
      if List.mem_assoc x pairs then return else Some (x, subst e pairs) in
  let ops' =
    List.map
      (fun (name, x, k, e) ->
         if List.mem_assoc x pairs || List.mem_assoc k pairs
         then (name, x, k, e)
         else (name, x, k, subst e pairs))
      ops in
  {return = return'; ops = ops'}

(* e の中の自由変数を値に置換する *)
and subst (e : e) (pairs : (string * v) list) : e = match e with
  | Val (v) -> Val (subst_value v pairs)
  | App (e1, e2) -> App (subst e1 pairs, subst e2 pairs)
  | Op (name, e0) -> Op (name, subst e0 pairs)
  | With (e1, e2) -> With (subst e1 pairs, subst e2 pairs)

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
let rec record_var_name : e -> unit = function
  | Val (v) -> record_var_name_value v
  | App (e1, e2) -> record_var_name e1; record_var_name e2
  | Op (op, e) -> add_var_name op; record_var_name e
  | With (e1, e2) -> record_var_name e1; record_var_name e2

and record_var_name_handler : h -> unit = fun {return; ops} ->
  begin match return with
    | None -> ()
    | Some (x, e) -> add_var_name x; record_var_name e
  end;
  List.iter
    (fun (name, x, k, e) ->
       add_var_name x; add_var_name k; record_var_name e)
    ops

and record_var_name_value : v -> unit = function
  | Fun (x, e) -> add_var_name x; record_var_name e
  | Handler (h) -> record_var_name_handler h
  | _ -> ()

(* プログラム内でまだ使われていない変数名を生成して返す *)
let gen_var_name () =
  let new_var =
    try List.find (fun var -> not (List.mem var !var_names)) var_name_examples
    with Not_found -> "x" ^ string_of_int (List.length !var_names) in
  var_names := new_var :: !var_names;
  new_var
