open Syntax

(* コンテキストフレームの型 *)
type cframe_t = CAppR of e_t   (* e [.] *)
              | CAppL of v_t   (* [.] v *)
              | CPlusR of e_t  (* e + [.] *)
              | CPlusL of v_t  (* [.] + v *)

(* 継続の型 *)
and c_t = CHole of cframe_t list
        | CReset of cframe_t list * c_t

let add (ctxt : c_t) (new_frame : cframe_t) : c_t = match ctxt with
  | CHole (frames) -> CHole (new_frame :: frames)
  | CReset (frames, outer_ctxt) -> CReset (new_frame :: frames, outer_ctxt)

let add_reset (ctxt : c_t) : c_t = CReset ([], ctxt)

(* 偽物の関数 *)
let mark_redex e = e
let mark_reduct e = e

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

let rec plug (expr : e_t) (ctxt : c_t) : e_t = match ctxt with
  | CHole (frames) -> plug_in_reset expr frames
  | CReset (frames, rest) -> plug (Reset (plug_in_reset expr frames)) rest

let step_counter = ref 0
let print_step () : unit =
  print_string ("Step " ^ string_of_int !step_counter ^ ":")

let memo (redex : e_t) (reduct : e_t) (ctxt : c_t) : unit =
  print_step ();
  print_exp (plug (mark_redex redex) ctxt);
  step_counter := 1 + !step_counter;
  print_step ();
  print_exp (plug (mark_reduct reduct) ctxt)
