type op_t = Read
          | Print

type defined_fun_t = Join

type pattern_t = PVar of string                  (* x *)
               | PPair of pattern_t * pattern_t  (* (p, p) *)

(* value の型 *)
type v_t = Var of string                   (* 変数 *)
         | True | False                    (* bool 型定数 *)
         | Fun of string * c_t             (* 関数 *)
         | Handler of h_t                  (* ハンドラ *)
         | String of string                (* 文字列定数 *)
         | Unit                            (* () *)
         | Pair of v_t * v_t               (* (v, v) *)

(* handler の型 *)
and h_t  = (string * c_t) option *              (* return 節 *)
           (op_t * string * string * c_t) list  (* op_n(x; k)->c_n *)

(* computation の型 *)
and c_t = Return of v_t                      (* 値 *)
        | Op of op_t * v_t * string * c_t    (* オペレーション呼び出し *)
        | Do of pattern_t * c_t * c_t        (* 束縛付き逐次実行 *)
        | Seq of c_t * c_t                   (* 逐次実行 *)
        | If of v_t * c_t * c_t              (* 条件分岐 *)
        | App of v_t * v_t                   (* 関数適用 *)
        | With of v_t * c_t                  (* ハンドリング *)
        | Op2 of defined_fun_t * v_t * v_t   (* 組み込み二項演算子 *)

(* 結合の優先順位（大きいほど弱くて括弧を付ける） *)
let prior_outside_value (v : v_t) : int = match v with
  | Fun _ -> 100
  | Handler _ -> 10
  | _ -> 0
let prior_outside (c : c_t) : int = match c with
  | Return _ -> 0
  | Op _ -> 0
  | Do _ -> 40
  | Seq _ -> 70
  | If _ -> 20
  | App _ -> 0
  | With _ -> 60
  | Op2 _ -> 55

let prior_handler_inside = 81

(* 結合の優先順位（大きいほど部分式に括弧を付けさせない） *)
let prior_inside (c : c_t) : int = match c with
  | Return (Fun _) -> 99
  | Return (Handler _) -> prior_handler_inside
  | Return _ -> 79
  | Op _ -> 65
  | Do _ -> 19
  | Seq _ -> 50
  | If _ -> 19
  | App _ -> 0
  | With _ -> 59
  | Op2 _ -> 0

let op_to_string (op : op_t) : string = match op with
  | Read -> "read"
  | Print -> "print"

let fun_to_string (f : defined_fun_t) : string = "join"

let rec pattern_to_string (pat : pattern_t) : string = match pat with
  | PVar (x) -> x
  | PPair (p1, p2) ->
    "(" ^ pattern_to_string p1 ^ ", " ^ pattern_to_string p2 ^ ")"

let rec handler_op_to_string (info : op_t * string * string * c_t) : string =
  match info with
  | (op, x, k, c) -> op_to_string op ^ "(" ^ x ^ "; " ^ k ^ ") -> "
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
    | Handler (h) -> handler_to_string h
    | String (s) -> "\"" ^ s ^ "\""
    | Unit -> "()"
    | Pair (v1, v2) -> "(" ^ v_to_string v1 p ^ ", " ^ v_to_string v2 p ^ ")"
  in
  if prior_outside_value v <= n
  then str
  else "(" ^ str ^ ")"

(* 式を受け取って文字列にする *)
and c_to_string (c : c_t) (n : int) : string =
  let p = prior_inside c in
  let str = match c with
    | Return (v) -> v_to_string v n
    | Op (op, v, y, c0) ->
      op_to_string op ^ "(" ^ v_to_string v p ^ "; "
      ^ y ^ ". " ^ c_to_string c0 p ^ ")"
    | Do (x, c1, c2) ->
      "do " ^ pattern_to_string x ^ " <- " ^ c_to_string c1 p ^
      " in " ^ c_to_string c2 p
    | Seq (c1, c2) ->
      c_to_string c1 p ^ "; " ^ c_to_string c2 p
    | If (v, c1, c2) ->
      "if " ^ v_to_string v p ^
      " then " ^ c_to_string c1 p ^
      " else " ^ c_to_string c2 p
    | App (v1, v2) ->
      v_to_string v1 p ^ " " ^ v_to_string v2 p
    | With (v, c0) ->
      "with " ^ v_to_string v p ^
      " handle " ^ c_to_string c0 p
    | Op2 (op, v1, v2) ->
      fun_to_string op ^ " " ^ v_to_string v1 p ^ " " ^ v_to_string v2 p
  in
  if prior_outside c <= n
  then str
  else "(" ^ str ^ ")"

let c_to_string (c : c_t) : string = c_to_string c 1000

(* 式を標準出力する *)
let print_computation (c : c_t) : unit =
  print_endline (c_to_string c)

(* 値を標準出力する *)
let print_value (v : v_t) : unit =
  print_endline (v_to_string v 1000)

