(* 値の型 *)
type v = Var of string       (* x *)
       | VFun of string * e  (* fun x -> e *)

(* 式の型 *)
and e = Val of v               (* v *)
      | Fun of string * e      (* fun x -> e *)
      | App of e * e           (* e e *)
      | Raise of e             (* raise *)
      | Try of e * string * e  (* try e with x -> e *)

and a = Value of v
      | Raised of v

and cont_in = FId
            | FApp2 of e * cont_in * cont_out
            (* e1, ctxt_in, ctxt_out, cont_in -> e1, cont_in, cont_out *)
            | FApp1 of v * cont_in * cont_out
            (* v2, ctxt_in, ctxt_out, cont_in -> v2, cont_in, cont_out *)
            | FRaise of cont_in * cont_out
(* ctxt_in, ctxt_out -> cont_in, cont_out *)

and cont_out = GId
             | GTry of
                 string * e * cont_in * cont_out
(* x, e2, ctxt_in, ctxt_out, cont_in, cont_out *)
(* -> x, e2, cont_in, cont_out *)

let rec plug_in_try (e : e) (cont_in : cont_in) : e = match cont_in with
  | FId -> e
  | FApp2 (e1, cont_in, cont_out) ->
    plug_in_try (App (e1, e)) cont_in
  | FApp1 (v2, cont_in, cont_out) ->
    plug_in_try (App (e, Val v2)) cont_in
  | FRaise (cont_in, cont_out) ->
    plug_in_try (Raise (e)) cont_in

let rec plug_all (e : e) ((cont_in, cont_out) : (cont_in * cont_out)) =
  let e_in_try = plug_in_try e cont_in in
  match cont_out with
  | GId -> e_in_try
  | GTry (x, e2, cont_in, cont_out) ->
    let e_try = Try (e_in_try, x, e2) in
    plug_all e_try (cont_in, cont_out)

(* 値を文字列にする関数 *)
let rec v_to_string (v : v) : string = match v with
  | Var (x) -> x
  | VFun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"

(* 式を文字列にする関数 *)
and e_to_string (e : e) : string = match e with
  | Val (v) -> v_to_string v
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"
  | App (e1, e2) -> "(" ^ e_to_string e1 ^ " " ^ e_to_string e2 ^ ")"
  | Raise (e) -> "(raise " ^ e_to_string e ^ ")"
  | Try (e1, x, e2) -> "(try " ^ e_to_string e1 ^
                       " with " ^ x ^ " -> " ^ e_to_string e2 ^ ")"

(* 式を標準出力する *)
let print_e (e : e) : unit =
  print_endline (e_to_string e)

(* 値を標準出力する *)
let print_v (v : v) : unit =
  print_endline (v_to_string v)

