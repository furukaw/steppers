open Syntax

(* 簡約回数を格納する変数 *)
let step_counter = ref 0

(* Step n: と標準出力する *)
let print_step () : unit =
  print_string ("Step " ^ string_of_int !step_counter ^ ":  ")

(* ステップを標準出力する *)
let memo : t -> t -> ctxt -> unit =
  fun redex reduct ctxt ->
    print_step ();
    print_t (plug redex ctxt);
    step_counter := 1 + !step_counter;
    print_step ();
    print_t (plug reduct ctxt)
