(* 値の型 *)
type v = Var of string      (* x *)
       | Fun of string * e  (* fun x -> e *)
       | Cont of (k -> k)

and h = {
  return : string * e;              (* handler {return x -> e,      *)
  ops : (string * string * string * e) list  (*          op(x; k) -> e, ...} *)
}

(* 式の型 *)
and e = Val of v          (* v *)
      | App of e * e      (* e e *)
      | Op of string * e  (* op e *)
      | With of h * e     (* with h handle e *)

and a = Return of v
      | OpCall of string * v * k

and k = FId
      | FApp2 of e * k
      | FApp1 of v * k
      | FOp of string * k
      | FCall of k * h * k

type k2 = GId
        | GHandle of k * h * k2

type cont = k * k2

let hole : e = Val (Var "8")

let rec plug_in_handle (e : e) (k : k) : e = match k with
  | FId -> e
  | FApp2 (e1, k) -> plug_in_handle (App (e1, e)) k
  | FApp1 (v2, k) -> plug_in_handle (App (e, Val v2)) k
  | FOp (name, k) -> plug_in_handle (Op (name, e)) k
  | FCall (k1, h, k2) ->
    plug_in_handle (With (h, plug_in_handle e k2)) k1

let rec plug_all (e : e) ((k, k2) : cont) : e =
  let e_in_handle = plug_in_handle e k in
  match k2 with
  | GId -> e_in_handle
  | GHandle (k, h, k2) ->
    let e_handle = With (h, e_in_handle) in
    plug_all e_handle (k, k2)

(* 値を文字列にする関数 *)
let rec v_to_string (v : v) : string = match v with
  | Var (x) -> x
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"
  | Cont (k) ->
    "(fun ? => " ^
    e_to_string (plug_in_handle (Val (Var "?")) (k FId)) ^ ")"

and h_to_string : h -> string = fun {return; ops} ->
  let return_strs = match return with (x, e) ->
    ["return " ^ x ^ " -> " ^ e_to_string e] in
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
  | With (h, e) ->
    "(with " ^ h_to_string h ^ " handle " ^ e_to_string e ^ ")"

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
