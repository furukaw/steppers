(* 値の型 *)
type v = Var of string        (* x *)
       | VFun of string * e  (* fun x -> e *)

(* 式の型 *)
and e = Val of v           (* v *)
      | Fun of string * e  (* fun x -> e *)
      | App of e * e       (* e e *)

(* 値を文字列にする関数 *)
let rec v_to_string (v : v) : string = match v with
  | Var (x) -> x
  | VFun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"

(* 式を文字列にする関数 *)
and e_to_string (e : e) : string = match e with
  | Val (v) -> v_to_string v
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"
  | App (e1, e2) -> "(" ^ e_to_string e1 ^ " " ^ e_to_string e2 ^ ")"

(* 式を標準出力する *)
let print_e (e : e) : unit =
  print_endline (e_to_string e)

(* 値を標準出力する *)
let print_v (v : v) : unit =
  print_endline (v_to_string v)

