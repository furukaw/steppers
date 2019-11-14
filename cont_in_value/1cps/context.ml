open Syntax

(* 簡約回数を格納する変数 *)
let step_counter = ref 0

(* Step n: と標準出力する *)
let print_step () : unit =
  print_string ("Step " ^ string_of_int !step_counter ^ ":  ")

(* ステップを標準出力する *)
let memo : e_t -> e_t -> ctx_t -> unit =
  fun redex reduct ctxt ->
    print_step ();
    print_e (plug redex ctxt);
    step_counter := 1 + !step_counter;
    print_step ();
    print_e (plug reduct ctxt)
