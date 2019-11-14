(* value の型 *)
type v_t = Var of string        (* x *)
         | True | False         (* true / false *)
         | Fun of string * c_t  (* fun x -> c *)
         | Handler of h_t       (* ハンドラ *)

(* handler の型 *)
and h_t  = (string * c_t) option *              (* return x -> c_r *)
           (string * string * string * c_t) list  (* op_n(x; k)->c_n *)

(* computation の型 *)
and c_t = Val of v_t             (* 値 *)
        | App of c_t * c_t       (* c c *)
        | If of c_t * c_t * c_t  (* if c then c else c *)
        | Op of string * c_t     (* op c *)
        | With of c_t * c_t      (* with c handle c *)
                  
(* 結合の優先順位（大きいほど弱くて括弧を付ける） *)
let prior_outside_value (v : v_t) : int = match v with
  | Fun _ -> 100
  | Handler _ -> 10
  | _ -> 0
let prior_outside (c : c_t) : int = match c with
  | Val _ -> 0
  | Op _ -> 0
  | If _ -> 20
  | App _ -> 0
  | With _ -> 60

let prior_handler_inside = 81

(* 結合の優先順位（大きいほど部分式に括弧を付けさせない） *)
let prior_inside (c : c_t) : int = match c with
  | Val (Fun _) -> 99
  | Val (Handler _) -> prior_handler_inside
  | Val _ -> 79
  | Op _ -> 80
  | If _ -> 19
  | App _ -> 0
  | With _ -> 59

let rec handler_op_to_string (info : string * string * string * c_t) : string =
  match info with
  | (op, x, k, c) -> op ^ "(" ^ x ^ "; " ^ k ^ ") -> "
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
  let p = prior_inside (Val v) in
  let str = match v with
    | Var (x) -> x
    | True -> "true"
    | False -> "false"
    | Fun (x, c0) -> "fun " ^ x ^ " -> " ^ c_to_string c0 p
    | Handler (h) -> handler_to_string h in
  if prior_outside_value v <= n
  then str
  else "(" ^ str ^ ")"

(* 式を受け取って文字列にする *)
and c_to_string (c : c_t) (n : int) : string =
  let p = prior_inside c in
  let str = match c with
    | Val (v) -> v_to_string v n
    | Op (op, c0) ->
      op ^ " " ^ c_to_string c0 p
    | If (c0, c1, c2) ->
      "if " ^ c_to_string c0 p ^
      " then " ^ c_to_string c1 p ^
      " else " ^ c_to_string c2 p
    | App (c1, c2) ->
      c_to_string c1 p ^ " " ^ c_to_string c2 p
    | With (v, c0) ->
      "with " ^ c_to_string c p ^
      " handle " ^ c_to_string c0 p
  in
  if prior_outside c <= n
  then str
  else "(" ^ str ^ ")"

(* 式を標準出力する *)
let print_computation (c : c_t) : unit =
  print_endline (c_to_string c 1000)

(* 値を標準出力する *)
let print_value (v : v_t) : unit =
  print_endline (v_to_string v 1000)

