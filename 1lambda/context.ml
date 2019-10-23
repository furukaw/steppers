open Syntax

(* コンテキストフレームの型 *)
type cframe_t = CAppL of e_t  (* F[e [.]] *)
              | CAppR of v_t  (* F[v [.]] *)

(* コンテキストの型 *)
type ctx_t = cframe_t list

(* コンテキストを１層深くする関数 *)
let add (frame : cframe_t) (ctxt : ctx_t) : ctx_t = frame :: ctxt

(* 式とその式のコンテキストを受け取って式全体を返す *)
let rec plug (e : e_t) (ctxt : ctx_t) = match ctxt with
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
let memo : e_t -> e_t -> ctx_t -> unit = fun redex reduct ctxt ->
    print_step ();
    print_e (plug redex ctxt);
    step_counter := 1 + !step_counter;
    print_step ();
    print_e (plug reduct ctxt)
