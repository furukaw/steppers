open Syntax

(* コンテキストフレームの型 *)
type cframe_t = CDo of string * c_t  (* do x <- [.] in c *)
              | CWith of v_t         (* with h handle [.] *)

(* 継続の型 *)
and ctx_t = cframe_t list

let rec plug_all (com : c_t) (ctxt : ctx_t) : c_t = match ctxt with
  | [] -> com
  | first :: rest ->
    begin match first with
      | CDo (x, c2) -> plug_all (Do (x, com, c2)) rest
      | CWith (h) -> plug_all (With (h, com)) rest
    end

(* 簡約回数を格納する変数 *)
let step_counter = ref 0

(* Step n: と標準出力する *)
let print_step () : unit =
  print_string ("Step " ^ string_of_int !step_counter ^ ":  ")

let memo (redex : c_t) (reduct : c_t) (ctxt : ctx_t) : unit =
  print_step ();
  print_computation (plug_all redex ctxt);
  step_counter := 1 + !step_counter;
  print_step ();
  print_computation (plug_all reduct ctxt)
