open Syntax

type cframe_t = CHole
              | CApp1 of c_t * cframe_t
              | CApp2 of v_t * cframe_t
              | CIf of c_t * c_t * cframe_t
              | COp of string * cframe_t
              | CWith of c_t * cframe_t

type chandle_t = CCore
               | CHandle of v_t * cframe_t * chandle_t

type ctx_t = cframe_t * chandle_t

let add_app1 (c : c_t) ((frames, handles) : ctx_t) : ctx_t =
  (CApp1 (c, frames), handles)
let add_app2 (v : v_t) ((frames, handles) : ctx_t) : ctx_t =
  (CApp2 (v, frames), handles)
let add_if (c1 : c_t) (c2 : c_t) ((frames, handles) : ctx_t) : ctx_t =
  (CIf (c1, c2, frames), handles)
let add_op (op : string) ((frames, handles) : ctx_t) : ctx_t =
  (COp (op, frames), handles)
let add_with (c : c_t) ((frames, handles) : ctx_t) : ctx_t =
  (CWith (c, frames), handles)

let add_handle (v : v_t) ((frames, handles) : ctx_t) : ctx_t =
  (CHole, CHandle (v, frames, handles))

let rec plug_frames (c : c_t) (frames : cframe_t) : c_t = match frames with
  | CHole -> c
  | CApp1 (c2, outer_frames) -> plug_frames (App (c, c2)) outer_frames
  | CApp2 (v1, outer_frames) -> plug_frames (App (Val v1, c)) outer_frames
  | CIf (c1, c2, outer_frames) -> plug_frames (If (c, c1, c2)) outer_frames
  | COp (op, outer_frames) -> plug_frames (Op (op, c)) outer_frames
  | CWith (c2, outer_frames) -> plug_frames (With (c, c2)) outer_frames

let rec plug_handles (c : c_t) (handles : chandle_t) : c_t = match handles with
  | CCore -> c
  | CHandle (v, outer_frames, outer_handles) ->
    let c_in_handle = plug_frames (With (Val v, c)) outer_frames in
    plug_handles c_in_handle outer_handles

let plug_all (c : c_t) ((frames, handles) : ctx_t) : c_t =
  let c_in_handle = plug_frames c frames in
  plug_handles c_in_handle handles

let rec plug_handles_op (op : string) (c : c_t) (handles : chandle_t) : c_t =
  match handles with
  | CCore -> failwith "plug_handles_op"
  | CHandle (v, outer_frames, outer_handles) ->
    begin match v with
      | Handler (_, ops)
        when List.exists (fun (name, x, k, c) -> name = op) ops -> c
      | _ ->
        plug_handles_op
          op
          (plug_frames (With (Val v, c)) outer_frames)
          outer_handles
    end

let plug_op (op : string) (c : c_t) ((frames, handles) : ctx_t) : c_t =
  plug_handles_op op (plug_frames c frames) handles

(* 簡約回数を格納する変数 *)
let step_counter = ref 0

(* Step n: と標準出力する *)
let print_step () : unit =
  print_string ("Step " ^ string_of_int !step_counter ^ ":  ")

let memo (redex : c_t) (reduct : c_t) (ctxt : ctx_t) : unit =
  print_step ();
  let program_before = plug_all redex ctxt in
  print_computation program_before;
  step_counter := 1 + !step_counter;
  flush_all ();
  print_step ();
  print_computation (plug_all reduct ctxt)
