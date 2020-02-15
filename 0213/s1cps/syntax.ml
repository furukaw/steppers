(* 二項演算子 *)
type binop = Plus | Minus | Times
(* 値 *)
type v = Var of string      (* x *)
       | Fun of string * e  (* fun x -> e *)
       | Cont of (k -> k)   (* 継続 fun x => ... *)
       | Int of int         (* n *)
       | Unit               (* () *)
(* ハンドラ *)
and h = {
  return : (string * e) option;              (* handler {return x -> e,      *)
  ops : (string * string * string * e) list  (*          op(x; k) -> e, ...} *)
}
(* 式 *)
and e = Val of v                (* v *)
      | App of e * e            (* e e *)
      | Op of string * e        (* op e *)
      | With of h * e           (* with h handle e *)
      | BinOp of e * binop * e  (* e + e *)
(* 継続 *)
and k = v -> a
(* 実行結果 *)
and a = Return of v
      | OpCall of string * v * k

let binop_to_string : binop -> string = function
  | Plus -> " + " | Minus -> " - " | Times -> " * "

(* 値を文字列にする関数 *)
let rec v_to_string (v : v) : string = match v with
  | Var (x) -> x
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"
  | Cont (k) -> "<cont>"
  | Int (n) -> string_of_int n
  | Unit -> "()"

and h_to_string : h -> string = fun {return; ops} ->
  let return_strs = match return with
    | None -> []
    | Some (x, e) -> ["return " ^ x ^ " -> " ^ e_to_string e] in
  let op_strs =
    List.map
      (fun (name, x, k, e) ->
         name ^ "(" ^ x ^ "; " ^ k ^ ") -> " ^ e_to_string e)
      ops in
  match return_strs @ op_strs with
  | [] -> ""
  | first :: rest ->
    List.fold_left (fun x y -> x ^ ", " ^ y) first rest

(* 式を文字列にする関数 *)
and e_to_string (e : e) : string = match e with
  | Val (v) -> v_to_string v
  | App (e1, e2) -> "(" ^ e_to_string e1 ^ " " ^ e_to_string e2 ^ ")"
  | Op (name, e) -> "(" ^ name ^ " " ^ e_to_string e ^ ")"
  | With (h, e) ->
    "(with " ^ h_to_string h ^ " handle " ^ e_to_string e ^ ")"
  | BinOp (e1, binop, e2) ->
    "(" ^ e_to_string e1 ^ binop_to_string binop ^ e_to_string e2 ^ ")"

let a_to_string : a -> string = function
  | Return v -> v_to_string v
  | OpCall (name, v, _) -> "(" ^ name ^ " " ^ v_to_string v ^ ")"

(* 式を標準出力する *)
let print_e (e : e) : unit =
  print_endline (e_to_string e)

(* 値を標準出力する *)
let print_v (v : v) : unit =
  print_endline (v_to_string v)

let print_a (a : a) : unit =
  print_endline (a_to_string a)
