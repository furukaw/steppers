type t = (string * Value.v) list

let empty = []

let get env key = List.assoc key env

let add env key value = (key, value) :: env
