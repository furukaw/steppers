(* 値の型 *)
type v = Var of string       (* x *)
       | Fun of string * e  (* fun x -> e *)
       | Cont of cont_in * string  (* fun x => e *)

(* 式の型 *)
and e = Val of v               (* v *)
      | App of e * e           (* e e *)
      | Shift of string * e    (* shift (fun x -> e) *)
      | Reset of e             (* reset (fun () -> e) *)

and cont_in = FId
            | FApp2 of e * cont_in  (* e1, cont_in, cont_out *)
            | FApp1 of v * cont_in  (* v2, cont_in, cont_out *)

and cont_out = GId
             | GReset of cont_in * cont_out (* cont_in, cont_out *)

and cont = cont_in * cont_out

let rec plug_in_reset (e : e) (cont_in : cont_in) : e = match cont_in with
  | FId -> e
  | FApp2 (e1, cont_in') -> plug_in_reset (App (e1, e)) cont_in'
  | FApp1 (v2, cont_in') -> plug_in_reset (App (e, Val v2)) cont_in'

let rec plug_all (e : e) ((cont_in, cont_out) : cont_in * cont_out) : e =
  let e_in_reset = plug_in_reset e cont_in in
  match cont_out with
  | GId -> e_in_reset
  | GReset (cont_next, cont_outer) ->
    let e_reset = Reset (e_in_reset) in
    plug_all e_reset (cont_next, cont_outer)

(* 値を文字列にする関数 *)
let rec v_to_string (v : v) : string = match v with
  | Var (x) -> x
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"
  | Cont (cont_in, x) ->
    "(fun " ^ x ^ " => " ^
    e_to_string (Reset (plug_in_reset (Val (Var x)) cont_in)) ^ ")"

(* 式を文字列にする関数 *)
and e_to_string (e : e) : string = match e with
  | Val (v) -> v_to_string v
  | App (e1, e2) -> "(" ^ e_to_string e1 ^ " " ^ e_to_string e2 ^ ")"
  | Shift (x, e) -> "(shift (fun " ^ x ^ " -> " ^ e_to_string e ^ "))"
  | Reset (e) -> "(reset (fun () -> " ^ e_to_string e ^ "))"

(* 式を標準出力する *)
let print_e (e : e) : unit =
  print_endline (e_to_string e)

(* 値を標準出力する *)
let print_v (v : v) : unit =
  print_endline (v_to_string v)

