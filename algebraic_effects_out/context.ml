open Syntax

(* with 以外のコンテキストの型 *)
type cframe_t = CHole
              | CApp1 of c_t * cframe_t
              | CApp2 of v_t * cframe_t
              | CIf of c_t * c_t * cframe_t
              | COp of string * cframe_t
              | CWith of c_t * cframe_t

type chandle_t = CCore
               | CHandle of v_t * cframe_t * chandle_t

type ctx_t = cframe_t * chandle_t

let rec add_frame (frame : cframe_t) (frames : cframe_t) : cframe_t =
  match frames with
  | CHole -> frame
  | CApp1 (c, inner_frames) -> CApp1 (c, add_frame frame inner_frames)
  | CApp2 (v, inner_frames) -> CApp2 (v, add_frame frame inner_frames)
  | CIf (c1, c2, inner_frames) -> CIf (c1, c2, add_frame frame inner_frames)
  | COp (op, inner_frames) -> COp (op, add_frame frame inner_frames)
  | CWith (c, inner_frames) -> CWith (c, add_frame frame inner_frames)

let rec add_frame_to_handle (frame : cframe_t) (handle : chandle_t) : chandle_t
  = match handle with
  | CCore -> failwith ("add_frame_to_handle")
  | CHandle (v, inner_frames, CCore) ->
    CHandle (v, add_frame frame inner_frames, CCore)
  | CHandle (v, inner_frames, inner_handle) ->
    CHandle (v, inner_frames, add_frame_to_handle frame inner_handle)

let rec add_frame_to_ctx (frame : cframe_t) ((frames, handles) : ctx_t) : ctx_t
  = match handles with
  | CCore -> (add_frame frame frames, handles)
  | CHandle (v, inner_frames, inner_handle) ->
    (frames, CHandle (v, inner_frames, add_frame_to_handle frame inner_handle))

let add_app1 (c : c_t) (ctx : ctx_t) : ctx_t =
  add_frame_to_ctx (CApp1 (c, CHole)) ctx
let add_app2 (v : v_t) (ctx : ctx_t) : ctx_t =
  add_frame_to_ctx (CApp2 (v, CHole)) ctx
let add_if (c1 : c_t) (c2 : c_t) (ctx : ctx_t) : ctx_t =
  add_frame_to_ctx (CIf (c1, c2, CHole)) ctx
let add_op (op : string) (ctx : ctx_t) : ctx_t =
  add_frame_to_ctx (COp (op, CHole)) ctx
let add_with (c : c_t) (ctx : ctx_t) : ctx_t =
  add_frame_to_ctx (CWith (c, CHole)) ctx

let add_handle (v : v_t) ((frames, handles) : ctx_t) : ctx_t =
  let rec add_handle (v : v_t) (handles : chandle_t) : chandle_t =
    match handles with
    | CCore -> CHandle (v, CHole, CCore)
    | CHandle (v_outer, outer_frames, rest_handles) ->
      CHandle (v_outer, outer_frames, add_handle v rest_handles) in
  (frames, add_handle v handles)

let rec plug_frames (c : c_t) (frames : cframe_t) : c_t = match frames with
  | CHole -> c
  | CApp1 (c2, inner_frames) -> App (plug_frames c inner_frames, c2)
  | CApp2 (v1, inner_frames) -> App (Val v1, plug_frames c inner_frames)
  | CIf (c1, c2, inner_frames) -> If (plug_frames c inner_frames, c1, c2)
  | COp (op, inner_frames) -> Op (op, plug_frames c inner_frames)
  | CWith (c1, inner_frames) -> With (plug_frames c inner_frames, c1)

let rec plug_handle (c : c_t) (handles : chandle_t) : c_t = match handles with
  | CCore -> c
  | CHandle (v, inner_frames, inner_handles) ->
    let next_handle = plug_handle c inner_handles in
    let c_in_handle = plug_frames next_handle inner_frames in
    With (Val v, c_in_handle)

let plug_all (c : c_t) ((frames, handles) : ctx_t) : c_t =
  let c_in_handle = plug_handle c handles in
  plug_frames c_in_handle frames

let rec pick_handles (op : string) (handles : chandle_t) : chandle_t =
  match handles with
  | CCore -> raise Not_found
  | CHandle (v, inner_frames, inner_handles) ->
    try
      CHandle (v, inner_frames, pick_handles)
 
let pick_context (op : string) ((frames, handles) : ctx_t) : ctx_t =
  (frames, pick_handles op handles)

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
