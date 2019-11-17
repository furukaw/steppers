open Syntax

(* コンテキストフレームの型 *)
type frame = CAppL of e  (* F[e [.]] *)
           | CAppR of v  (* F[v [.]] *)

(* コンテキストの型 *)
type ctxt = frame list

(* コンテキストを１層深くする関数 *)
let add_frame (frame : frame) (ctxt : ctxt) : ctxt = frame :: ctxt

(* 式とその式のコンテキストを受け取って式全体を返す *)
let rec plug_all (e : e) (ctxt : ctxt) = match ctxt with
  | [] -> e
  | first :: rest ->
    let plugged = match first with
      | CAppL (e2) -> App (e, e2)
      | CAppR (v1) -> App (Val (v1), e) in
    plug_all plugged rest
