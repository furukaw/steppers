(* 値の型 *)
type v = Var of string       (* x *)
       | VFun of string * e  (* fun x -> e *)

(* 式の型 *)
and e = Val of v           (* v *)
      | Fun of string * e  (* fun x -> e *)
      | App of e * e       (* e e *)

and cont = v -> v

(* コンテキストフレームの型 *)
and frame = CApp2 of e  (* F[e [.]] *)
          | CApp1 of v  (* F[[.] v] *)

(* コンテキストの型 *)
and ctxt = frame list

(* コンテキストを１層深くする関数 *)
let add_frame (frame : frame) (ctxt : ctxt) : ctxt = frame :: ctxt

(* 式とその式のコンテキストを受け取って式全体を返す *)
let rec plug_all (e : e) (ctxt : ctxt) = match ctxt with
  | [] -> e
  | first :: rest ->
    let plugged = match first with
      | CApp2 (e1) -> App (e1, e)
      | CApp1 (v2) -> App (e, Val v2) in
    plug_all plugged rest

(* 値を文字列にする関数 *)
let rec v_to_string (v : v) : string = match v with
  | Var (x) -> x
  | VFun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"

(* 式を文字列にする関数 *)
and e_to_string (e : e) : string = match e with
  | Val (v) -> v_to_string v
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"
  | App (e1, e2) -> "(" ^ e_to_string e1 ^ " " ^ e_to_string e2 ^ ")"

(* 式を標準出力する *)
let print_e (e : e) : unit =
  print_endline (e_to_string e)

(* 値を標準出力する *)
let print_v (v : v) : unit =
  print_endline (v_to_string v)

