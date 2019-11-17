
type t = Val of v
       | Fun of string * t
       | App of t * t
       | Shift of string * t
       | Reset of t

and v = Var of string
      | VFun of ((v -> ctxt -> cont -> v) * (string * t))
      | VCont of cont * (string * t)

and cont = v -> v

and frame = App2 of t
          | App1 of v

and ctxt = (frame list * frame list list)

let empty_context = ([], [])

let add_frame (frame : frame) (frames : frame list) : frame list =
  frame :: frames

let add_reset (frames : frame list) (framess : frame list list)
  : frame list list = frames :: framess

let rec plug_frames (t : t) (frames : frame list) = match frames with
  | [] -> t
  | first :: rest ->
    let plugged = match first with
      | App2 (t1) -> App (t1, t)
      | App1 (v2) -> App (t, Val v2) in
    plug_frames plugged rest

let rec plug (t : t) ((frames_innermost, resets) : ctxt) : t =
  let t_in_reset = plug_frames t frames_innermost in
  match resets with
  | [] -> t_in_reset
  | first :: rest -> plug (Reset t_in_reset) (first, rest)

let rec t_to_string : t -> string = function
  | Val (v) -> v_to_string v
  | Fun (x, t) -> "(fun " ^ x ^ " -> " ^ t_to_string t ^ ")"
  | App (t1, t2) -> "(" ^ t_to_string t1 ^ " " ^ t_to_string t2 ^ ")"
  | Shift (x, t) -> "(shift (fun " ^ x ^ " -> " ^ t_to_string t ^ "))"
  | Reset (t) -> "(reset (fun () -> " ^ t_to_string t ^ "))"

and v_to_string : v -> string = function
  | Var (x) -> x
  | VFun (_, (x, t)) -> "(fun " ^ x ^ " -> " ^ t_to_string t ^ ")"
  | VCont (_, (x, t)) -> "(fun " ^ x ^ " => " ^ t_to_string t ^ ")"

let print_t (t : t) : unit =
  print_endline (t_to_string t)

let print_v (v : v) : unit =
  print_endline (v_to_string v)
