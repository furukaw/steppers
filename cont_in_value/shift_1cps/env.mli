open Syntax
open Value

type t
val empty : t
val get : t -> string -> v
val add : t -> string -> v -> t

