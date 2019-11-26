(* 値の型 *)
type v = Var of string       (* x *)
       | VFun of string * e  (* fun x -> e *)

(* 式の型 *)
and e = Val of v               (* v *)
      | Fun of string * e      (* fun x -> e *)
      | App of e * e           (* e e *)
      | Raise of e             (* raise *)
      | Try of e * string * e  (* try e with x -> e *)

and a = Value of v
      | Raised of v

and cont_in = FId
            | FApp2 of e * frame list * try_frame list * cont_in
            (* e1, ctxt_in, ctxt_out, cont *)
            | FApp1 of v * frame list * try_frame list * cont_in
            (* v2, ctxt_in, ctxt_out, cont *)
            | FRaise of frame list * try_frame list (* ctxt_in, ctxt_out *)

and cont_out = GId
             | GTry of
                 string * e * frame list * try_frame list * cont_in * cont_out
                 (* x, e2, ctxt_in, ctxt_out, cont_in, cont_out *)
                 
(* コンテキストフレームの型 *)
and frame = CApp2 of e  (* F[e [.]] *)
          | CApp1 of v  (* F[[.] v] *)
          | CRaise      (* F[raise [.]] *)

and try_frame = CTry of frame list * string * e (* E[F[try [.] with x -> e]] *)

(* コンテキストの型 *)
and ctxt = frame list * try_frame list  (* E[F[]] *)

(* コンテキストを１層深くする関数 *)
let add_frame (frame : frame) (frames : frame list) : frame list =
  frame :: frames

let add_try (x : string) (e : e)
    (frames : frame list) (framess : try_frame list) : try_frame list =
  (CTry (frames, x, e)) :: framess

let rec plug_in_try (e : e) (frames : frame list) : e = match frames with
  | [] -> e
  | first :: rest ->
    let plugged = match first with
      | CApp2 (e1) -> App (e1, e)
      | CApp1 (v2) -> App (e, Val v2)
      | CRaise -> Raise (e) in
    plug_in_try plugged rest

(* 式とその式のコンテキストを受け取って式全体を返す *)
let rec plug_all (e : e) ((frames, framess) : ctxt) : e =
  let e_in_try = plug_in_try e frames in
  match framess with
  | [] -> e_in_try
  | CTry (outer_frames, x, e_with) :: rest ->
    let e_try = Try (e_in_try, x, e_with) in
    plug_all e_try (outer_frames, rest)

(* 値を文字列にする関数 *)
let rec v_to_string (v : v) : string = match v with
  | Var (x) -> x
  | VFun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"

(* 式を文字列にする関数 *)
and e_to_string (e : e) : string = match e with
  | Val (v) -> v_to_string v
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ e_to_string e ^ ")"
  | App (e1, e2) -> "(" ^ e_to_string e1 ^ " " ^ e_to_string e2 ^ ")"
  | Raise (e) -> "(raise " ^ e_to_string e ^ ")"
  | Try (e1, x, e2) -> "(try " ^ e_to_string e1 ^
                       " with " ^ x ^ " -> " ^ e_to_string e2 ^ ")"

(* 式を標準出力する *)
let print_e (e : e) : unit =
  print_endline (e_to_string e)

(* 値を標準出力する *)
let print_v (v : v) : unit =
  print_endline (v_to_string v)

