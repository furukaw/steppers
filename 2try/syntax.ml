(* 値の型 *)
type v_t = Var of string        (* x *)
         | Fun of string * e_t  (* fun x -> e *)

(* 式の型 *)
and e_t = Val of v_t                 (* v *)
        | App of e_t * e_t           (* e e *)
        | Raise of e_t               (* raise e *)
        | Try of e_t * string * e_t  (* try e with x -> e *)

(* 値を文字列にする関数 *)
let rec v_to_string : v_t -> string = function
  | Var (x) -> x
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"

(* 式を文字列にする関数 *)
and e_to_string : e_t -> string = function
  | Val (v) -> v_to_string v
  | App (e1, e2) -> "(" ^ e_to_string e1 ^ " " ^ e_to_string e2 ^ ")"
  | Raise (e) -> "(raise " ^ e_to_string e ^ ")"
  | Try (e0, x, e1) -> "(try " ^ e_to_string e0 ^ " with " ^ x ^ " -> "
                       ^ e_to_string e1 ^ ")"

(* 式を標準出力する *)
let print_e (e : e_t) : unit =
  print_endline (e_to_string e)

(* 値を標準出力する *)
let print_v (v : v_t) : unit =
  print_endline (v_to_string v)

