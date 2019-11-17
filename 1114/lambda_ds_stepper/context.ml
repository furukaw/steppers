open Syntax

(* コンテキストフレームの型 *)
type cframe_t = CAppL of e  (* F[e [.]] *)
              | CAppR of v  (* F[v [.]] *)

(* コンテキストの型 *)
type ctxt = cframe_t list

(* コンテキストを１層深くする関数 *)
let add (frame : cframe_t) (ctxt : ctxt) : ctxt = frame :: ctxt

(* 式とその式のコンテキストを受け取って式全体を返す *)
let rec plug (e : e) (ctxt : ctxt) = match ctxt with
  | [] -> e
  | first :: rest ->
    let plugged = match first with
      | CAppL (e2) -> App (e, e2)
      | CAppR (v1) -> App (Val (v1), e) in
    plug plugged rest

(* 簡約回数を格納する変数 *)
let step_counter = ref 0

(* Step n: と標準出力する *)
let print_step () : unit =
  print_string ("Step " ^ string_of_int !step_counter ^ ":  ")

(* ステップを標準出力する *)
let memo : e -> e -> ctxt -> unit = fun redex reduct ctxt ->
    print_step ();
    print_e (plug redex ctxt);
    step_counter := 1 + !step_counter;
    print_step ();
    print_e (plug reduct ctxt)
