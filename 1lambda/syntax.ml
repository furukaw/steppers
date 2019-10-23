(* 値の型 *)
type v_t = Var of string        (* x *)
         | Fun of string * e_t  (* fun x -> e *)

(* 式の型 *)
and e_t = Val of v_t            (* v *)
        | App of e_t * e_t      (* e e *)

(* 値を文字列にする関数 *)
let rec v_to_string (v : v_t) : string = match v with
  | Var (x) -> x
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"

(* 式を文字列にする関数 *)
and e_to_string (e : e_t) : string = match e with
  | Val (v) -> v_to_string v
  | App (e1, e2) -> "(" ^ e_to_string e1 ^ " " ^ e_to_string e2 ^ ")"

(* 式を標準出力する *)
let print_e (e : e_t) : unit =
  print_endline (e_to_string e)

(* 値を標準出力する *)
let print_v (v : v_t) : unit =
  print_endline (v_to_string v)

