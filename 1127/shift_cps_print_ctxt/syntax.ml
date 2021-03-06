(* 値の型 *)
type v = Var of string       (* x *)
       | Fun of string * e  (* fun x -> e *)
       | Cont of cont * ctxt_out * (string * e)  (* fun x => e *)

(* 式の型 *)
and e = Val of v               (* v *)
      | App of e * e           (* e e *)
      | Shift of string * e    (* shift (fun x -> e) *)
      | Reset of e             (* reset (fun () -> e) *)

and cont = v -> ctxt_out -> v

(* コンテキストフレームの型 *)
and frame = CApp2 of e  (* F[e [.]] *)
          | CApp1 of v  (* F[[.] v] *)

and reset_frame = CReset of frame list
(* E[F[reset (fun () -> [.])]] *)

and ctxt_in = frame list
and ctxt_out = reset_frame list
and ctxt = frame list * reset_frame list  (* E[F[]] *)

(* コンテキストを１層深くする関数 *)
let add_frame (frame : frame) (frames : frame list) : frame list =
  frame :: frames

let add_reset
    (frames : frame list) (framess : reset_frame list) : reset_frame list =
  (CReset (frames)) :: framess

 let rec plug_in_reset (e : e) (frames : frame list) : e = match frames with
  | [] -> e
  | first :: rest ->
    let plugged = match first with
      | CApp2 (e1) -> App (e1, e)
      | CApp1 (v2) -> App (e, Val v2) in
    plug_in_reset plugged rest

(* 式とその式のコンテキストを受け取って式全体を返す *)
let rec plug_all (e : e) ((frames, framess) : ctxt) : e =
  let e_in_reset = plug_in_reset e frames in
  match framess with
  | [] -> e_in_reset
  | CReset (outer_frames) :: rest ->
    let e_reset = Reset (e_in_reset) in
    plug_all e_reset (outer_frames, rest)

(* 値を文字列にする関数 *)
let rec v_to_string (v : v) : string = match v with
  | Var (x) -> x
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"
  | Cont (_, _, (x, e)) -> "(fun " ^ x ^ " => " ^ e_to_string e ^ ")"

(* 式を文字列にする関数 *)
and e_to_string (e : e) : string = match e with
  | Val (v) -> v_to_string v
  | App (e1, e2) -> "(" ^ e_to_string e1 ^ " " ^ e_to_string e2 ^ ")"
  | Shift (x, e) -> "(shift (fun " ^ x ^ " -> " ^ e_to_string e ^ "))"
  | Reset (e) -> "(reset (fun () -> " ^ e_to_string e ^ "))"

(* 式を標準出力する *)
let print_e (e : e) : unit =
  print_endline (e_to_string e)

(* 値を標準出力する *)
let print_v (v : v) : unit =
  print_endline (v_to_string v)

let frame_to_string : frame -> string = function
  | CApp2 (e1) -> "CApp2 " ^ e_to_string e1
  | CApp1 (v2) -> "CApp1 " ^ v_to_string v2

let ctxt_in_to_string (ctxt_in : ctxt_in) : string =
  match List.map frame_to_string ctxt_in with 
  | [] -> "(no frames)"
  | first :: rest ->
    List.fold_left (fun x y -> x ^ ", " ^ y) first rest
      
let ctxt_out_to_string (ctxt_out : ctxt_out) : string =
  match List.map (fun (CReset (ctxt_in)) ->
      "(Reset " ^ ctxt_in_to_string ctxt_in ^ ")") ctxt_out with
  | [] -> "(no resets)"
  | first :: rest ->
    List.fold_left (fun x y -> x ^ "\n    " ^ y) first rest

let print_ctxt (ctxt_in, ctxt_out) : unit =
  print_string "in : ";
  print_endline (ctxt_in_to_string ctxt_in);
  print_string "out: ";
  print_endline (ctxt_out_to_string ctxt_out);
  
