open Syntax

(* コンテキストフレームの型 *)
type cframe_t = CDo of pattern_t * c_t  (* do x <- [.] in c *)
              | CSeq of c_t             (* [.]; c *)
              | CWith of v_t            (* with h handle [.] *)

(* 継続の型 *)
and ctx_t = cframe_t list

type cvalue_t = CApp1 of v_t * ctx_t  (* v [.] *)
              | CReturn of ctx_t (* Return [.] *)
              | CPair1 of v_t * cvalue_t  (* ([.], v) *)
              | CPair2 of v_t * cvalue_t  (* (v, [.]) *)

let rec plug_all (com : c_t) (ctxt : ctx_t) : c_t = match ctxt with
  | [] -> com
  | first :: rest ->
    let plugged = match first with
      | CDo (x, c2) -> Do (x, com, c2)
      | CSeq (c2) -> Seq (com, c2)
      | CWith (h) -> With (h, com)
    in plug_all plugged rest

let rec plug_to_value (value : v_t) (ctxt : cvalue_t) : c_t = match ctxt with
  | CApp1 (v, rest) -> plug_all (App (v, value)) rest
  | CReturn (rest) -> plug_all (Return value) rest
  | CPair1 (v2, rest) -> plug_to_value (Pair (value, v2)) rest
  | CPair2 (v1, rest) -> plug_to_value (Pair (v1, value)) rest

type store_t = (string list) ref

let printed : store_t = ref []
let read : store_t = ref []

(* 簡約回数を格納する変数 *)
let step_counter = ref 0

(* Step n: と標準出力する *)
let print_step () : unit =
  print_string ("Step " ^ string_of_int !step_counter ^ ":  ")

let print_stored (r : store_t) (header : string) (indent : int) : bool =
  begin
    match !r with
    | [] -> false
    | first :: rest ->
      let spaces = "\n" ^ String.make indent ' ' in
      print_string header;
      print_endline
        (List.fold_left
           (fun all oldest -> oldest ^ spaces ^ all)
           first
           rest);
      true
  end

let print_io (newline : bool) : unit =
  let a = print_stored read "  Input:   " 11 in
  let b = print_stored printed "  Output:  " 11 in
  if newline && (a || b)
  then print_newline ()
  else ()

let append (r : store_t) (content : string) : unit =
  r := content :: !r

let memo : c_t -> c_t -> ?input:string option -> ?output:string option -> ctx_t
  -> unit = fun redex reduct ?(input=None) ?(output=None) ctxt ->
  print_step ();
  print_computation (plug_all redex ctxt);
  print_io false;
  step_counter := 1 + !step_counter;
  begin match input with None -> () | Some str -> append read str end;
  begin match output with None -> () | Some str -> append printed str end;
  print_step ();
  print_computation (plug_all reduct ctxt);
  print_io true

let memo_for_value : v_t -> v_t -> cvalue_t -> unit = fun redex reduct ctxt ->
  print_step ();
  print_computation (plug_to_value redex ctxt);
  print_io false;
  step_counter := 1 + !step_counter;
  print_step ();
  print_computation (plug_to_value reduct ctxt);
  print_io true
