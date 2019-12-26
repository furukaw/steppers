
(* 値の型 *)
type v = Var of string      (* x *)
       | Num of int         (* n *)
       | Fun of string * e  (* fun x -> e *)
       | Handler of h
       | Cont of string * (ctxt_in -> ctxt_in)

and h = {
  return : (string * e) option;              (* handler {return x -> e,      *)
  ops : (string * string * string * e) list  (*          op(x; k) -> e, ...} *)
}

(* 式の型 *)
and e = Val of v          (* v *)
      | App of e * e      (* e e *)
      | Op of string * e  (* op e *)
      | With of e * e     (* with e handle e *)

and ctxt_in = CId
            | CApp2 of e * ctxt_in
            | CApp1 of v * ctxt_in
            | COp of string * ctxt_in
            | CWith of e * ctxt_in
            | CCall of ctxt_in * h * ctxt_in

and ctxt_out = DId
             | DHandle of ctxt_in * h * ctxt_out

type ctxt = ctxt_in * ctxt_out

exception Call of string * v * ctxt_in

type a = Return of v
       | OpCall of string * v * ctxt_in

let rec plug_in_handle (e : e) (ctxt_in : ctxt_in) : e = match ctxt_in with
  | CId -> e
  | CApp2 (e1, ctxt_in) -> plug_in_handle (App (e1, e)) ctxt_in
  | CApp1 (v2, ctxt_in) -> plug_in_handle (App (e, Val v2)) ctxt_in
  | COp (name, ctxt_in) -> plug_in_handle (Op (name, e)) ctxt_in
  | CWith (e2, ctxt_in) -> plug_in_handle (With (e, e2)) ctxt_in
  | CCall (ctxt_in1, h, ctxt_in2) ->
    plug_in_handle (With (Val (Handler h), plug_in_handle e ctxt_in2)) ctxt_in1

let rec plug_all (e : e) ((ctxt_in, ctxt_out) : ctxt) : e =
  let e_in_handle = plug_in_handle e ctxt_in in
  match ctxt_out with
  | DId -> e_in_handle
  | DHandle (ctxt_in, h, ctxt_out) ->
    let e_handle = With (Val (Handler h), e_in_handle) in
    plug_all e_handle (ctxt_in, ctxt_out)

(* 値を文字列にする関数 *)
let rec v_to_string (v : v) : string = match v with
  | Var (x) -> x
  | Num (n) -> string_of_int n
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"
  | Handler (h) -> "(handler {" ^ h_to_string h ^ "})"
  | Cont (x, cont_in) -> "<cont>"

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
