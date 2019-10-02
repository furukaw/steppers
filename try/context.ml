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
  | CNoTry of cframe_t                     (* F[.] *)
  | CTry of string * e_t * cframe_t * c_t  (* try F[.] with x -> e *)

(* 空のコンテキスト *)
let empty = CNoTry (CHole)

(* 例外発生時に捨てられるコンテキストフレームがあるかどうかを返す *)
let exists_frames_in_try (ctxt : c_t) : bool = match ctxt with
  | CNoTry (CHole) -> false
  | CNoTry (_) -> true
  | CTry (_, _, CHole, _) -> false
  | CTry (_, _, _, _) -> true

(* コンテキストにフレームを追加する関数群 *)
let add_app1 (e : e_t) (ctxt : c_t) : c_t = match ctxt with
  | CNoTry (frames) -> CNoTry (CApp1 (e, frames))
  | CTry (x, e1, inner, outer) -> CTry (x, e1, CApp1 (e, inner), outer)
let add_app2 (v : v_t) (ctxt : c_t) : c_t = match ctxt with
  | CNoTry (frames) -> CNoTry (CApp2 (v, frames))
  | CTry (x, e1, inner, outer) -> CTry (x, e1, CApp2 (v, inner), outer)
let add_if (e1 : e_t) (e2 : e_t) (ctxt : c_t) : c_t = match ctxt with
  | CNoTry (frames) -> CNoTry (CIf (e1, e2, frames))
  | CTry (x, et, inner, outer) -> CTry (x, e1, CIf (e1, e2, inner), outer)
let add_raise (ctxt : c_t) : c_t = match ctxt with
  | CNoTry (frames) -> CNoTry (CRaise (frames))
  | CTry (x, e, inner, outer) -> CTry (x, e, CRaise (inner), outer)
let add_try (x : string) (e : e_t) (ctxt : c_t) : c_t =
  CTry (x, e, CHole, ctxt)

(* 式と try 以外のコンテキストを受け取って再構成した式を返す *)
let rec plug_frames (e : e_t) (frames : cframe_t) : e_t = match frames with
  | CHole -> e
  | CApp1 (e2, rest) -> plug_frames (App (e, e2)) rest
  | CApp2 (v1, rest) -> plug_frames (App (Val v1, e)) rest
  | CIf (e1, e2, rest) -> plug_frames (If (e, e1, e2)) rest
  | CRaise (rest) -> plug_frames (Raise e) rest

(* 式とコンテキストを受け取って、一番内側の try より内側の式を再構成して返す *)
let plug_in_try (e : e_t) (ctxt : c_t) : e_t =
  let ctxt_in_try = match ctxt with
    | CNoTry (frames) -> frames
    | CTry (x, e, frames, outer_ctxt) -> frames in
  plug_frames e ctxt_in_try

(* 式とコンテキストを受け取ってプログラム全体を再構成して返す *)
let rec plug_all (e : e_t) (ctxt : c_t) : e_t = match ctxt with
  | CNoTry (frames) -> plug_frames e frames
  | CTry (x, et, in_try, try_and_outer) ->
    plug_all (Try ((plug_frames e in_try), x, et)) try_and_outer

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
