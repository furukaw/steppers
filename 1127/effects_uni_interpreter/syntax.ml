(* 値の型 *)
type v = Var of string       (* x *)
       | Fun of string * e  (* fun x -> e *)
       | Cont of cont_in * (string * e)
       | Handler of h

(* 式の型 *)
and e = Val of v           (* v *)
      | App of e * e       (* e e *)
      | Op of string * e   (* op e *)
      | With of e * e      (* with e handle e *)

and h = {
  return : (string * e) option;
  ops : (string * string * string * e) list;
}

and a = Return of v
      | OpCall of string * v * cont_in * cont_out * cont_out

and cont_in = FId
         | FApp2 of e * cont_in * cont_out     (* e1, cont_in, cont_out *)
         | FApp1 of v * cont_in * cont_out     (* v2, cont_in, cont_out *)
         | FOp of string * cont_in * cont_out  (* op, cont_in, cont_out *)
         | FWith of e * cont_in * cont_out     (* e2, cont_in, cont_out *)
         | FCont of cont_in * cont_in

and cont_out = GId
             | GHandle of h * cont_in * cont_out  (* h, cont_in, cont_out *)

type cont = cont_in * cont_out

let rec add_handle_out (cont_out : cont_out) (h2 : h) (cont_in2 : cont_in) (cont_out2 : cont_out) : cont_out =
  match cont_out with
  | GId -> GHandle (h2, cont_in2, GId)
  | GHandle (h1, cont_in1, cont_out1) ->
    GHandle (h1, cont_in1, add_handle_out cont_out1 h2 cont_in2 cont_out2)

let rec plug_in_handle (e : e) (cont_in : cont_in) : e = match cont_in with
  | FId -> e
  | FApp2 (e1, cont_in, cont_out) ->
    plug_in_handle (App (e1, e)) cont_in
  | FApp1 (v2, cont_in, cont_out) ->
    plug_in_handle (App (e, Val v2)) cont_in
  | FOp (op, cont_in, cont_out) ->
    plug_in_handle (Op (op, e)) cont_in
  | FWith (e2, cont_in, cont_out) ->
    plug_in_handle (With (e, e2)) cont_in
  | FCont (cont_in1, cont_in2) ->
    plug_in_handle (plug_in_handle e cont_in1) cont_in2

let rec plug_all (e : e) ((cont_in, cont_out) : cont_in * cont_out) : e =
  let e_in_handle = plug_in_handle e cont_in in
  match cont_out with
  | GId -> e_in_handle
  | GHandle (h, cont_in, cont_out) ->
    let e_handle = With (Val (Handler h), e_in_handle) in
    plug_all e_handle (cont_in, cont_out)

(* 値を文字列にする関数 *)
let rec v_to_string (v : v) : string = match v with
  | Var (x) -> x
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"
  | Cont (_, (x, e)) -> "(fun " ^ x ^ " => " ^ e_to_string e ^ ")"
  | Handler (h) -> "(handler {" ^ h_to_string h ^ "})"

(* 式を文字列にする関数 *)
and e_to_string (e : e) : string = match e with
  | Val (v) -> v_to_string v
  | App (e1, e2) -> "(" ^ e_to_string e1 ^ " " ^ e_to_string e2 ^ ")"
  | Op (op, e1) -> "(" ^ op ^ " " ^ e_to_string e1 ^ ")"
  | With (e1, e2) ->
    "(with " ^ e_to_string e1 ^ " handle " ^ e_to_string e2 ^ ")"

and h_to_string : h -> string = function
  | {return; ops} ->
    let op_strs =
      List.map
        (fun (op, x, k, e) -> op ^ "(" ^ x ^ "; " ^ k ^ ") -> " ^ e_to_string e)
        ops in
    let strs = match return with
      | None -> op_strs
      | Some (x, e) -> ("return " ^ x ^ " -> " ^ e_to_string e) :: op_strs in
    match strs with
    | [] -> ""
    | first :: rest ->
    List.fold_left
      (fun x y -> x ^ ", " ^ y)
      first
      rest

(* 式を標準出力する *)
let print_e (e : e) : unit =
  print_endline (e_to_string e)

(* 値を標準出力する *)
let print_v (v : v) : unit =
  print_endline (v_to_string v)
