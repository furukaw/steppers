open Syntax

type frame = App2 of t
           | App1 of v

type ctxt = (frame list * frame list list)

let empty = ([], [])

let add_frame (frame : frame) ((frames_innermost, resets) : ctxt) : ctxt =
  (frame :: frames_innermost, resets)

let add_reset ((frames, resets) : ctxt) : ctxt =
  ([], frames :: resets)

let rec plug_frames (t : t) (frames : frame list) = match frames with
  | [] -> t
  | first :: rest ->
    let plugged = match first with
      | App2 (t1) -> App (t1, t)
      | App1 (v2) -> App (t, Val v2) in
    plug_frames plugged rest

let plug_in_reset (t : t) ((frames_in_reset, resets) : ctxt) : t =
  plug_frames t frames_in_reset

let rec plug (t : t) ((frames_innermost, resets) : ctxt) : t =
  let t_in_reset = plug_frames t frames_innermost in
  match resets with
  | [] -> t_in_reset
  | first :: rest -> plug t_in_reset (first, rest)

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
