open Syntax

(* コンテキストフレームの型 *)
type cf_t = CAppR of e_t   (* F[e [.]] *)
          | CAppL of v_t   (* F[[.] v] *)
          | COp of string  (* F[op [.]] *)
          | CWith of e_t   (* F[with [.] handle e] *)

type ce_t = CHandle of cf_t list * h_t  (* E[F[with v handle [.]]] *)

(* コンテキストの型 *)
type c_t = cf_t list * ce_t list  (* E[F[]] *)

let empty : c_t = ([], [])  (* F[], E[] *)

(* コンテキストを１層深くする関数 *)
let add_frame (frame : cf_t) ((frames, handles) : c_t) : c_t =
  (frame :: frames, handles)

let add_handle (h : h_t) ((frames, handles) : c_t) =
  ([], (CHandle (frames, h)) :: handles)

let rec plug_frames (e : e_t) (frames : cf_t list) : e_t = match frames with
  | [] -> e
  | first :: rest ->
    let plugged = match first with
      | CAppR (e1) -> App (e1, e)
      | CAppL (v2) -> App (e, Val v2)
      | COp (name) -> Op (name, e)
      | CWith (e2) -> With (e, e2) in
    plug_frames plugged rest

let rec plug (e : e_t) ((frames, handles) : c_t) : e_t =
  let e_in_handle = plug_frames e frames in
  match handles with
  | [] -> e_in_handle
  | CHandle (outer_frames, h) :: outer_handles ->
    plug (With (Val (Handler h), e)) (outer_frames, outer_handles)

let rec plug_in_handles name (e : e_t) ((frames, handles) : c_t) : e_t =
  let e_in_handle = plug_frames e frames in
  match handles with
  | [] -> e_in_handle
  | CHandle (outer_frames, ({return; ops} as h)) :: outer_handles ->
    if List.exists (fun (op, x, k, e) -> op = name) ops
    then e_in_handle
    else plug_in_handles
        name (With (Val (Handler h), e_in_handle)) (outer_frames, outer_handles)

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
