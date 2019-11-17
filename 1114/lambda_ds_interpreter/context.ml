open Syntax

(* コンテキストフレームの型 *)
type frame = CApp2 of e  (* F[e [.]] *)
           | CApp1 of v  (* F[[.] v] *)

(* コンテキストの型 *)
type ctxt = frame list

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
