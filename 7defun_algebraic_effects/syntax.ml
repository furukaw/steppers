(* 値の型 *)
type v_t = Var of string        (* x *)
         | Fun of string * e_t  (* fun x -> e *)
         | Handler of h_t

and h_t = {
  return : (string * e_t) option;  (* handler {return x -> er, *)
  ops : (string * string * string * e_t) list  (* op1(x; k) -> c1, ...} *)
}

(* 式の型 *)
and e_t = Val of v_t                 (* v *)
        | App of e_t * e_t           (* e e *)
        | Op of string * e_t         (* op e *)
        | With of e_t * e_t          (* with e handle e *)

(* 値を文字列にする関数 *)
let rec v_to_string : v_t -> string = function
  | Var (x) -> x
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"
  | Handler (h) -> "(handler {" ^ h_to_string h ^ "})"

and h_to_string : h_t -> string = fun {return; ops} ->
  let return_str : string list = match return with
    | None -> []
    | Some (x, er) -> ["return " ^ x ^ " -> " ^ e_to_string er] in
  let op_to_string (name, x, k, e) =
    name ^ "(" ^ x ^ "; " ^ k ^ ") -> " ^ e_to_string e in
  let ops_str = List.map op_to_string ops in
  match (return_str @ ops_str) with
  | [] -> ""
  | first :: rest ->
    List.fold_left (fun x y -> x ^ ", " ^ y) first rest

(* 式を文字列にする関数 *)
and e_to_string : e_t -> string = function
  | Val (v) -> v_to_string v
  | App (e1, e2) -> "(" ^ e_to_string e1 ^ " " ^ e_to_string e2 ^ ")"
  | Op (name, e) -> "(" ^ name ^ " " ^ e_to_string e ^ ")"
  | With (e1, e2) ->
    "(with " ^ e_to_string e1 ^ " handle " ^ e_to_string e2 ^ ")"

(* 式を標準出力する *)
let print_e (e : e_t) : unit =
  print_endline (e_to_string e)

(* 値を標準出力する *)
let print_v (v : v_t) : unit =
  print_endline (v_to_string v)

