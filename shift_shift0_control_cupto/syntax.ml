(* 値の型 *)
type v_t = Lam of string * e_t  (* λx.e *)
         | Num of int           (* 1 *)

(* 式の型 *)
and e_t = Value of v_t
        | Var of string             (* x *)
        | App of e_t * e_t          (* e e *)
        | Plus of e_t * e_t         (* e + e *)
        | Reset of e_t              (* reset (fun () -> e) *)
        | Shift of string * e_t     (* shift (fun k -> e) *)
        | Shift0 of string * e_t    (* shift0 (fun k -> e) *)
        | Control of string * e_t   (* control (fun k -> e) *)
        | Cupto of string * e_t     (* cupto (fun k -> e) *)

(* 結合の優先順位（大きいほど弱くて括弧を付ける（普通逆か）） *)
let prior (e : e_t) : int = match e with
  | Value (Lam _) -> 80
  | Value (Num _) -> 0
  | Var _ -> 0
  | App _ -> 40
  | Plus _ -> 60
  | Reset _ -> 70
  | Shift _ -> 70
  | Shift0 _ -> 70
  | Control _ -> 70
  | Cupto _ -> 70

(* 値を受け取って文字列にする *)
let rec v_to_string (v : v_t) : string = match v with
  | Lam (x, e) -> "fun " ^ x ^ " -> " ^ e_to_string e (prior (Value v))
  | Num (n) -> string_of_int n

(* 式を受け取って文字列にする *)
and e_to_string (e : e_t) (n : int): string =
  let p = prior e in
  let str = match e with
    | Value (v) -> v_to_string v
    | Var (x) -> x
    | App (e1, e2) -> e_to_string e1 p ^ " " ^ e_to_string e2 (p - 1)
    | Plus (e1, e2) -> e_to_string e1 p ^ " + " ^ e_to_string e2 (p - 1)
    | Reset (e) -> "reset (fun () -> " ^ e_to_string e p ^ ")"
    | Shift (k, e) -> "shift (fun " ^ k ^ " -> " ^ e_to_string e p ^ ")"
    | Shift0 (k, e) -> "shift0 (fun " ^ k ^ " -> " ^ e_to_string e p ^ ")"
    | Control (k, e) -> "control (fun " ^ k ^ " -> " ^ e_to_string e p ^ ")"
    | Cupto (k, e) -> "cupto (fun " ^ k ^ " -> " ^ e_to_string e p ^ ")"
  in
  if p <= n
  then str
  else "(" ^ str ^ ")"

(* 式を標準出力する *)
let print_exp (e : e_t) : unit =
  print_string "  ";
  print_endline (e_to_string e 100)

(* 値を標準出力する *)
let print_value (v : v_t) : unit =
  print_string "  ";
  print_endline (v_to_string v)

