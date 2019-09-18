(* value の型 *)
type v_t = Var of string        (* 変数 *)
         | True | False         (* bool 型定数 *)
         | Fun of string * c_t  (* 関数 *)
         | Handler of h_t       (* ハンドラ *)

(* handler の型 *)
and h_t  = (string * c_t) option *                (* return 節 *)
           (string * string * string * c_t) list  (* op_n(x; k)->c_n *)

(* computation の型 *)
and c_t = Return of v_t                      (* 値 *)
        | Op of string * v_t * string * c_t  (* オペレーション呼び出し *)
        | Do of string * c_t * c_t           (* 逐次実行 *)
        | If of v_t * c_t * c_t              (* 条件分岐 *)
        | App of v_t * v_t                   (* 関数適用 *)
        | With of v_t * c_t                  (* ハンドリング *)

(* 結合の優先順位（大きいほど弱くて括弧を付ける） *)
let prior_outside (c : c_t) : int = match c with
  | Return (Fun _) -> 100
  | Return (Handler _) -> 80
  | Return _ -> 0
  | Op _ -> 0
  | Do _ -> 40
  | If _ -> 20
  | App _ -> 0
  | With _ -> 60

let prior_handler_inside = 81

(* 結合の優先順位（大きいほど部分式に括弧を付けさせない） *)
let prior_inside (c : c_t) : int = match c with
  | Return (Fun _) -> 99
  | Return (Handler _) -> prior_handler_inside
  | Return _ -> 79
  | Op _ -> 1
  | Do _ -> 19
  | If _ -> 19
  | App _ -> 0
  | With _ -> 59

let rec handler_op_to_string (info : string * string * string * c_t) : string =
  match info with
  | (op, x, k, c) -> op ^ " (" ^ x ^ "; " ^ k ^ " -> "
                     ^ c_to_string c prior_handler_inside

and handler_to_string (h : h_t) : string =
  match h with (return_opt, op_lst) ->
    let return_str = match return_opt with
      | None -> ""
      | Some (x, c) ->
        "return " ^ x ^ " -> " ^ c_to_string c prior_handler_inside in
    let ops_str = match op_lst with
      | [] -> ""
      | first :: rest ->
        List.fold_left
          (fun str op -> handler_op_to_string op ^ ", " ^ str)
          (handler_op_to_string first)
          rest in
    let comma = if return_str <> "" && ops_str <> "" then ", " else "" in
    "handler {" ^ return_str ^ comma ^ ops_str ^ "}"

and v_to_string (v : v_t) (n : int) : string =
  let p = prior_inside (Return v) in
  let str = match v with
    | Var (x) -> x
    | True -> "true"
    | False -> "false"
    | Fun (x, c0) -> "fun " ^ x ^ " -> " ^ c_to_string c0 p
    | Handler (h) -> handler_to_string h in
  if prior_outside (Return v) <= n
  then str
  else "(" ^ str ^ ")"

(* 式を受け取って文字列にする *)
and c_to_string (c : c_t) (n : int)  : string =
  let p = prior_inside c in
  let str = match c with
    | Return (v) -> v_to_string v p
    | Op (name, v, y, c0) ->
      name ^ "(" ^ v_to_string v p ^ "; "
      ^ y ^ ". " ^ c_to_string c0 p ^ ")"
    | Do (x, c1, c2) ->
      "do " ^ x ^ " <- " ^ c_to_string c1 p ^
      " in " ^ c_to_string c2 p
    | If (v, c1, c2) ->
      "if " ^ v_to_string v p ^
      " then " ^ c_to_string c1 p ^
      " else " ^ c_to_string c2 p
    | App (v1, v2) ->
      v_to_string v1 p ^ " " ^ v_to_string v2 p
    | With (v, c0) ->
      "with " ^ v_to_string v p ^
      " handle " ^ c_to_string c0 p
  in
  if prior_outside c <= n
  then str
  else "(" ^ str ^ ")"

(* 式を標準出力する *)
let print_computation (c : c_t) : unit =
  print_endline (c_to_string c 100)

(* 値を標準出力する *)
let print_value (v : v_t) : unit =
  print_endline (v_to_string v 100)

