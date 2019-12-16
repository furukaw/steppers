(* 値の型 *)
type v = Var of string       (* x *)
       | Fun of string * e   (* fun x -> e *)
       | Cont of string * ctxt_in * cont_in  (* fun x => e *)

(* 式の型 *)
and e = Val of v               (* v *)
      | App of e * e           (* e e *)
      | Shift of string * e    (* shift (fun x -> e) *)
      | Reset of e             (* reset (fun () -> e) *)

and cont_in = FId
            | FApp2 of e * ctxt_in * cont_in  (* e1, ctxt_in, cont_in *)
            | FApp1 of v * ctxt_in * cont_in  (* v2, ctxt_in, cont_in *)

and cont_out = v -> v

and cont = cont_in * cont_out

(* コンテキストフレームの型 *)
and frame = CApp2 of e  (* F[e [.]] *)
          | CApp1 of v  (* F[[.] v] *)

and reset_frame = CReset of frame list
(* E[F[reset (fun () -> [.])]] *)

and ctxt_in = frame list
and ctxt_out = reset_frame list
and ctxt = ctxt_in * ctxt_out  (* E[F[]] *)

let rec plug_in_reset (e : e) (frames : frame list) : e = match frames with
  | [] -> e
  | first :: rest ->
    let plugged = match first with
      | CApp2 (e1) -> App (e1, e)
      | CApp1 (v2) -> App (e, Val v2) in
    plug_in_reset plugged rest

(* 式とその式のコンテキストを受け取って式全体を返す *)
let rec plug_all (e : e) ((frames, framess) : ctxt) : e =
  let e_in_reset = plug_in_reset e frames in
  match framess with
  | [] -> e_in_reset
  | CReset (outer_frames) :: rest ->
    let e_reset = Reset (e_in_reset) in
    plug_all e_reset (outer_frames, rest)

(* 値を文字列にする関数 *)
let rec v_to_string (v : v) : string = match v with
  | Var (x) -> x
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"
  | Cont (x, ctxt_in, cont_in) ->
    "(fun " ^ x ^ " => "
    ^ e_to_string (Reset (plug_in_reset (Val (Var x)) ctxt_in)) ^ ")"

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

(* let rec plug_in_reset (e : e) (cont_in : cont_in) : e = match cont_in with
 *   | FId -> e
 *   | FApp2 (e1, ctxt_in, cont_in) ->
 *     plug_in_reset (App (e1, e)) cont_in
 *   | FApp1 (v2, ctxt_in, cont_in) ->
 *     plug_in_reset (App (e, Val v2)) cont_in
 * 
 * let rec plug_all (e : e) ((cont_in, cont_out) : cont) : e =
 *   let e_in_reset = plug_in_reset e cont_in in
 *   match cont_out with
 *   | GId -> e_in_reset
 *   | GReset (_, (cont_in, cont_out)) ->
 *     let e_reset = Reset (e_in_reset) in
 *     plug_all e_reset (cont_in, cont_out) *)
