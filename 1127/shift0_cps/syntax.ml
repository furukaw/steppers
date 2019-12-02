(* 値の型 *)
type v = Var of string       (* x *)
       | VFun of string * e  (* fun x -> e *)
       | VCont of cont * (string * e)  (* fun x => e *)

(* 式の型 *)
and e = Val of v               (* v *)
      | Fun of string * e      (* fun x -> e *)
      | App of e * e           (* e e *)
      | Shift0 of string * e    (* shift0 (fun x -> e) *)
      | Reset0 of e             (* reset0 (fun () -> e) *)

and cont = v -> v

(* コンテキストフレームの型 *)
and frame = CApp2 of e  (* F[e [.]] *)
          | CApp1 of v  (* F[[.] v] *)

and reset0_frame = CReset0 of frame list
(* E[F[reset0 (fun () -> [.])]] *)

(* コンテキストの型 *)
and ctxt = frame list * reset0_frame list  (* E[F[]] *)

(* コンテキストを１層深くする関数 *)
let add_frame (frame : frame) (frames : frame list) : frame list =
  frame :: frames

let add_reset0
    (frames : frame list) (framess : reset0_frame list) : reset0_frame list =
  (CReset0 (frames)) :: framess

let rec plug_in_reset0 (e : e) (frames : frame list) : e = match frames with
  | [] -> e
  | first :: rest ->
    let plugged = match first with
      | CApp2 (e1) -> App (e1, e)
      | CApp1 (v2) -> App (e, Val v2) in
    plug_in_reset0 plugged rest

(* 式とその式のコンテキストを受け取って式全体を返す *)
let rec plug_all (e : e) ((frames, framess) : ctxt) : e =
  let e_in_reset0 = plug_in_reset0 e frames in
  match framess with
  | [] -> e_in_reset0
  | CReset0 (outer_frames) :: rest ->
    let e_reset0 = Reset0 (e_in_reset0) in
    plug_all e_reset0 (outer_frames, rest)

(* 値を文字列にする関数 *)
let rec v_to_string (v : v) : string = match v with
  | Var (x) -> x
  | VFun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"
  | VCont (_, (x, e)) -> "(fun " ^ x ^ " => " ^ e_to_string e ^ ")"

(* 式を文字列にする関数 *)
and e_to_string (e : e) : string = match e with
  | Val (v) -> v_to_string v
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"
  | App (e1, e2) -> "(" ^ e_to_string e1 ^ " " ^ e_to_string e2 ^ ")"
  | Shift0 (x, e) -> "(shift0 (fun " ^ x ^ " -> " ^ e_to_string e ^ "))"
  | Reset0 (e) -> "(reset0 (fun () -> " ^ e_to_string e ^ "))"

(* 式を標準出力する *)
let print_e (e : e) : unit =
  print_endline (e_to_string e)

(* 値を標準出力する *)
let print_v (v : v) : unit =
  print_endline (v_to_string v)

