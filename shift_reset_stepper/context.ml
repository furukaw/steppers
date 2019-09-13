open Syntax

(* コンテキストフレームの型 *)
type cframe_t = CAppR of e_t   (* e [.] *)
              | CAppL of v_t   (* [.] v *)
              | CPlusR of e_t  (* e + [.] *)
              | CPlusL of v_t  (* [.] + v *)

(* 継続の型 *)
and c_t = CHole of cframe_t list
        | CReset of cframe_t list * c_t

(* コンテキストフレームを足す *)
let add (ctxt : c_t) (new_frame : cframe_t) : c_t = match ctxt with
  | CHole (frames) -> CHole (new_frame :: frames)
  | CReset (frames, outer_ctxt) -> CReset (new_frame :: frames, outer_ctxt)

let add_reset (ctxt : c_t) : c_t = CReset ([], ctxt)

(* 式に attribute をつけるということになっている偽物の関数 *)
let mark_redex e = e
let mark_reduct e = e

(* 式とコンテキストを受け取って reset に囲まれた式を再構成して返す *)
let rec plug_in_reset (expr : e_t) (ctxt : cframe_t list) : e_t =
  match ctxt with
  | [] -> expr
  | first :: rest ->
    let expr' = match first with
      | CAppR (e1) -> App (e1, expr)
      | CAppL (v2) -> App (expr, Value (v2))
      | CPlusR (e1) -> Plus (e1, expr)
      | CPlusL (v2) -> Plus (expr, Value (v2)) in
    plug_in_reset expr' rest

(* 式とコンテキストを受け取ってプログラム全体を再構成して返す *)
let rec plug (expr : e_t) (ctxt : c_t) : e_t = match ctxt with
  | CHole (frames) -> plug_in_reset expr frames
  | CReset (frames, rest) -> plug (Reset (plug_in_reset expr frames)) rest

(* 簡約回数を格納する変数 *)
let step_counter = ref 0

(* Step n: と標準出力する *)
let print_step () : unit =
  print_string ("Step " ^ string_of_int !step_counter ^ ":")

(* 簡約基とその簡約後の式と簡約時のコンテキストを受け取ってその簡約ステップを標準出力する *)
let memo (redex : e_t) (reduct : e_t) (ctxt : c_t) : unit =
  print_step ();
  print_exp (plug (mark_redex redex) ctxt);
  step_counter := 1 + !step_counter;
  print_step ();
  print_exp (plug (mark_reduct reduct) ctxt)
