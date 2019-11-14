open Syntax
open Value

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
      | App1 (v2) -> App (t, v_to_t v2) in
    plug_frames plugged rest

let plug_in_reset (t : t) ((frames_innermost, resets) : ctxt) : t =
  plug_frames t frames_innermost
