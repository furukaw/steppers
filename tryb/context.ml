open Syntax

(* try 以外のコンテキストの型 *)
type cframe_t =
  | CHole                                  (* [.] *)
  | CApp1 of e_t * cframe_t                (* [.] e *)
  | CApp2 of v_t * cframe_t                (* v [.] *)
  | CIf of e_t * e_t * cframe_t            (* if [.] then e else e *)
  | CRaise of cframe_t                     (* raise [.] *)

(* コンテキストの型 *)
type c_t =
  | CSkin                                  (* 外の果て *)
  | CCore of cframe_t * c_t                (* 一番内側の try の中 *)
  | CTry of string * e_t * cframe_t * c_t  (* F[try [.] with x -> e] *)

(* 空のコンテキスト *)
let empty = CSkin

(* デバッグ用、コンテキストを文字列にする関数 *)
let rec frames_to_string (frame : cframe_t) (inner : string) : string =
  match frame with
  | CHole -> inner
  | CApp1 (e2, outer) ->
    frames_to_string outer ("[" ^ inner ^ " " ^ e_to_string e2 ^ "]")
  | CApp2 (v1, outer) ->
    frames_to_string outer ("[" ^ v_to_string v1 ^ " " ^ inner ^ "]")
  | CIf (e1, e2, outer) ->
    frames_to_string outer
      ("[if " ^ inner ^ " then " ^ e_to_string e1 ^
       " else " ^ e_to_string e2 ^ "]")
  | CRaise (outer) ->
    frames_to_string outer ("[raise " ^ inner ^ "]")
let rec ctxt_to_string (ctxt : c_t) (inner : string): string = match ctxt with
  | CSkin -> inner
  | CCore (frames, outer) ->
    ctxt_to_string outer (frames_to_string frames inner)
  | CTry (x, e, frames, outer_try) ->
    ctxt_to_string outer_try
      (frames_to_string frames
         ("[try " ^ inner ^ " with " ^ x ^ " -> " ^ e_to_string e ^ "]"))
(* デバッグ用、コンテキストを標準出力する関数 *)
let print_ctxt (ctxt : c_t) : unit = print_endline (ctxt_to_string ctxt "[ ]")

(* 例外が発生したときに捨てられるコンテキストがあるかどうかを返す *)
let exists_frames_in_try (ctxt : c_t) : bool = match ctxt with
  | CSkin -> false
  | CCore _ -> true
  | CTry _ -> false

(* コンテキストを１層深くする関数群 *)
let add_app1 (e : e_t) (ctxt : c_t) : c_t = match ctxt with
  | CSkin -> CCore (CApp1 (e, CHole), CSkin)
  | CCore (frames, outer) -> CCore (CApp1 (e, frames), outer)
  | CTry (x, et, frames, outer_try) -> CCore (CApp1 (e, CHole), ctxt)
let add_app2 (v : v_t) (ctxt : c_t) : c_t = match ctxt with
  | CSkin -> CCore (CApp2 (v, CHole), CSkin)
  | CCore (frames, outer) -> CCore (CApp2 (v, frames), outer)
  | CTry (x, e, frames, outer_try) -> CCore (CApp2 (v, CHole), ctxt)
let add_if (e1 : e_t) (e2 : e_t) (ctxt : c_t) : c_t = match ctxt with
  | CSkin -> CCore (CIf (e1, e2, CHole), CSkin)
  | CCore (frames, outer) -> CCore (CIf (e1, e2, frames), outer)
  | CTry (x, e, frames, outer_try) -> CCore (CIf (e1, e2, CHole), ctxt)
let add_raise (ctxt : c_t) : c_t = match ctxt with
  | CSkin -> CCore (CRaise (CHole), CSkin)
  | CCore (frames, outer) -> CCore (CRaise (frames), outer)
  | CTry (x, e, frames, outer_try) -> CCore (CRaise (CHole), ctxt)
let add_try (x : string) (e : e_t) (ctxt : c_t) : c_t = match ctxt with
  | CSkin -> CTry (x, e, CHole, CSkin)
  | CCore (frames, outer) -> CTry (x, e, frames, outer)
  | CTry (x1, e1, frames, outer_try) -> CTry (x, e, CHole, ctxt)

(* 式と try 内部のコンテキストを受け取って再構成した式を返す *)
let rec plug_frames (e : e_t) (frames : cframe_t) : e_t = match frames with
  | CHole -> e
  | CApp1 (e2, rest) -> plug_frames (App (e, e2)) rest
  | CApp2 (v1, rest) -> plug_frames (App (Val v1, e)) rest
  | CIf (e1, e2, rest) -> plug_frames (If (e, e1, e2)) rest
  | CRaise (rest) -> plug_frames (Raise e) rest

(* 式とコンテキストを受け取って一番内側の try の中の式を再構成して返す *)
let plug_in_try (e : e_t) (ctxt : c_t) : e_t =
  let ctxt_in_try = match ctxt with
    | CSkin -> CHole
    | CCore (frames, outer) -> frames
    | CTry (x, e, frames, outer_try) -> frames in
  plug_frames e ctxt_in_try

(* 式とコンテキストを受け取ってプログラム全体を再構成して返す *)
let rec plug_all (e : e_t) (ctxt : c_t) : e_t = match ctxt with
  | CSkin -> e
  | CCore (frames, outer) -> plug_all (plug_frames e frames) outer
  | CTry (x, et, frames, outer_try) ->
    plug_all (plug_frames (Try (e, x, et)) frames) outer_try

(* 簡約回数を格納する変数 *)
let step_counter = ref 0

(* Step n: と標準出力する *)
let print_step () : unit =
  print_string ("Step " ^ string_of_int !step_counter ^ ":  ")

(* ステップを標準出力する *)
let memo : e_t -> e_t -> c_t -> unit =
  fun redex reduct ctxt ->
  print_step ();
  print_e (plug_all redex ctxt);
  step_counter := 1 + !step_counter;
  print_step ();
  print_e (plug_all reduct ctxt)
