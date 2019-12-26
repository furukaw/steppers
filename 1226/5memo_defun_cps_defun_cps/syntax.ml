(* 値の型 *)
type v = Var of string      (* x *)
       | Num of int         (* n *)
       | Fun of string * e  (* fun x -> e *)
       | Handler of h
       | Cont of string * (cont_in -> cont_in)

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
      | OpCall of string * v * cont_in

and cont_in = FId
            | FApp2 of e * cont_in
            | FApp1 of v * cont_in
            | FOp of string * cont_in
            | FWith of e * cont_in
            | FCall of cont_in * h * cont_in
and cont_out = GId
             | GHandle of cont_in * h * cont_out

type cont = cont_in * cont_out

let hole : e = Val (Var "8")

let rec plug_in_handle (e : e) (cont_in : cont_in) : e = match cont_in with
  | FId -> e
  | FApp2 (e1, cont_in) -> plug_in_handle (App (e1, e)) cont_in
  | FApp1 (v2, cont_in) -> plug_in_handle (App (e, Val v2)) cont_in
  | FOp (name, cont_in) -> plug_in_handle (Op (name, e)) cont_in
  | FWith (e2, cont_in) -> plug_in_handle (With (e, e2)) cont_in
  | FCall (cont_in1, h, cont_in2) ->
    plug_in_handle (With (Val (Handler h), plug_in_handle e cont_in2)) cont_in1

let rec plug_all (e : e) ((cont_in, cont_out) : cont) : e =
  let e_in_handle = plug_in_handle e cont_in in
  match cont_out with
  | GId -> e_in_handle
  | GHandle (cont_in, h, cont_out) ->
    let e_handle = With (Val (Handler h), e_in_handle) in
    plug_all e_handle (cont_in, cont_out)

(* 値を文字列にする関数 *)
let rec v_to_string (v : v) : string = match v with
  | Var (x) -> x
  | Num (n) -> string_of_int n
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"
  | Handler (h) -> h_to_string h
  | Cont (x, cont_in) ->
    "(fun " ^ x ^ " => " ^
    e_to_string (plug_in_handle (Val (Var x)) (cont_in FId)) ^ ")"

and h_to_string : h -> string = fun {return; ops} ->
  let return_strs = match return with
    | None -> []
    | Some (x, e) -> ["return " ^ x ^ " -> " ^ e_to_string e] in
  let op_strs =
    List.map
      (fun (name, x, k, e) ->
         name ^ "(" ^ x ^ "; " ^ k ^ ") -> " ^ e_to_string e)
      ops in
  let content = match return_strs @ op_strs with
  | [] -> ""
  | first :: rest ->
    List.fold_left (fun x y -> x ^ ", " ^ y) first rest in
  "(handler {" ^ content ^ "})"

(* 式を文字列にする関数 *)
and e_to_string (e : e) : string = match e with
  | Val (v) -> v_to_string v
  | App (e1, e2) -> "(" ^ e_to_string e1 ^ " " ^ e_to_string e2 ^ ")"
  | Op (name, e) -> "(" ^ name ^ " " ^ e_to_string e ^ ")"
  | With (e1, e2) ->
    "(with " ^ e_to_string e1 ^ " handle " ^ e_to_string e2 ^ ")"

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
