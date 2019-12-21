(* 値の型 *)
type v = Var of string      (* x *)
       | Num of int         (* n *)
       | Fun of string * e  (* fun x -> e *)
       | Handler of h
       | Cont of cont_in

and h = {
  return : (string * e) option;              (* handler {return x -> e,      *)
  ops : (string * string * string * e) list  (*          op(x; k) -> e, ...} *)
}

(* 式の型 *)
and e = Val of v          (* v *)
      | App of e * e      (* e e *)
      | Op of string * e  (* op e *)
      | With of e * e     (* with e handle e *)

and a = Return of v
      | OpCall of string * v * cont_in * (h * cont_in) list

and cont_in = v -> a

(* 値を文字列にする関数 *)
let rec v_to_string (v : v) : string = match v with
  | Var (x) -> x
  | Num (n) -> string_of_int n
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"
  | Handler (h) -> "(handler {" ^ h_to_string h ^ "})"
  | Cont (cont_in) -> "<cont>"

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
  | With (e1, e2) ->
    "(with " ^ e_to_string e1 ^ " handle " ^ e_to_string e2 ^ ")"

(* 式を標準出力する *)
let print_e (e : e) : unit =
  print_endline (e_to_string e)

(* 値を標準出力する *)
let print_v (v : v) : unit =
  print_endline (v_to_string v)

