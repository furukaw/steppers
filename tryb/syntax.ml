(* 値の型 *)
type v_t = Var of string        (* x *)
         | Fun of string * e_t  (* fun x -> e *)
         | True                 (* true *)
         | False                (* false *)

(* 式の型 *)
and e_t = Val of v_t                 (* v *)
        | App of e_t * e_t           (* e e *)
        | If of e_t * e_t * e_t      (* if e then e else e *)
        | Raise of e_t               (* raise e *)
        | Try of e_t * string * e_t  (* try e with x -> e *)

(* プログラム実行結果の型 *)
type a_t = Ok of v_t     (* 正常終了 *)
         | Error of v_t  (* 例外終了 *)

(* 結合の優先順位（大きいほど弱くて括弧を付ける） *)
let prior_outside_value (v : v_t) : int = match v with
  | Fun _ -> 100
  | _ -> 0
let prior_outside (e : e_t) : int = match e with
  | Val _ -> 0
  | App _ -> 0
  | If _ -> 20
  | Raise _ -> 0
  | Try _ -> 20

(* 結合の優先順位（大きいほど部分式に括弧を付けさせない） *)
let prior_inside_value (v : v_t) : int = match v with
  | Fun _ -> 99
  | _ -> 0
let prior_inside (e : e_t) : int = match e with
  | Val v -> prior_inside_value v
  | App _ -> 0
  | If _ -> 19
  | Raise _ -> 0
  | Try _ -> 10

(* 値を文字列にする関数 *)
let rec v_to_string (v : v_t) (n : int) : string =
  let p = prior_inside_value v in
  let str = match v with
    | Var (x) -> x
    | True -> "true"
    | False -> "false"
    | Fun (x, e) -> "fun " ^ x ^ " -> " ^ e_to_string e p
  in
  if prior_outside_value v <= n
  then str
  else "(" ^ str ^ ")"

(* 式を文字列にする関数 *)
and e_to_string (e : e_t) (n : int) : string =
  let p = prior_inside e in
  let str = match e with
    | Val (v) -> v_to_string v n
    | App (e1, e2) ->
      e_to_string e1 p ^ " " ^ e_to_string e2 p
    | If (e0, e1, e2) ->
      "if " ^ e_to_string e0 p ^
      " then " ^ e_to_string e1 p ^
      " else " ^ e_to_string e2 p
    | Raise (e0) ->
      "raise " ^ e_to_string e0 p
    | Try (e0, x, e1) ->
      "try " ^ e_to_string e0 p ^
      " with " ^ x ^ " -> " ^ e_to_string e1 p
  in
  if prior_outside e <= n
  then str
  else "(" ^ str ^ ")"

(* 値/式/実行結果を文字列にする関数 *)
let v_to_string (v : v_t) : string = v_to_string v 1000
let e_to_string (e : e_t) : string = e_to_string e 1000
let a_to_string (a : a_t) : string = match a with
  | Ok (v) -> v_to_string v
  | Error (v) -> "raise " ^ v_to_string v

(* 値を標準出力する *)
let print_v (v : v_t) : unit =
  print_endline (v_to_string v)

(* 式を標準出力する *)
let print_e (e : e_t) : unit =
  print_endline (e_to_string e)

(* 実行結果を標準出力する *)
let print_a (a : a_t) : unit =
  print_endline (a_to_string a)

