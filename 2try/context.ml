open Syntax

(* コンテキストフレームの型 *)
type cf_t = CAppR of e_t  (* F[e [.]] *)
          | CAppL of v_t  (* F[[.] v] *)
          | CRaise        (* F[raise [.]] *)

type ce_t = CTry of cf_t list * string * e_t  (* E[F[try [.] with x -> e]] *)

(* コンテキストの型 *)
type c_t = cf_t list * ce_t list  (* E[F[]] *)

let empty = ([], [])

(* コンテキストを１層深くする関数 *)
let add_frame (frame : cf_t) ((frames, tries) : c_t) : c_t =
  (frame :: frames, tries)

let add_try (x : string) (e : e_t) ((frames, tries) : c_t) =
  ([], (CTry (frames, x, e)) :: tries)

let rec plug_frames (e : e_t) (frames : cf_t list) : e_t = match frames with
  | [] -> e
  | first :: rest ->
    let plugged = match first with
      | CAppR (e1) -> App (e1, e)
      | CAppL (v2) -> App (e, Val v2)
      | CRaise -> Raise (e) in
    plug_frames plugged rest

let rec plug (e : e_t) ((frames, tries) : c_t) : e_t =
  let e_in_try = plug_frames e frames in
  match tries with
  | [] -> e_in_try
  | CTry (outer_frames, x, e_with) :: outer_tries ->
    plug (Try (e, x, e_with)) (outer_frames, outer_tries)

let plug_in_try (e : e_t) ((frames, tries) : c_t) : e_t =
  plug_frames e frames

(* 簡約回数を格納する変数 *)
let step_counter = ref 0

(* Step n: と標準出力する *)
let print_step () : unit =
  print_string ("Step " ^ string_of_int !step_counter ^ ":  ")

(* ステップを標準出力する *)
let memo : e_t -> e_t -> c_t -> unit =
  fun redex reduct ctxt ->
    print_step ();
    print_e (plug redex ctxt);
    step_counter := 1 + !step_counter;
    print_step ();
    print_e (plug reduct ctxt)
