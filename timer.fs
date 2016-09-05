module Timer

open Fable.Core

[<Emit("window.setTimeout($0,$1)")>]
let setTimeout : (unit -> unit) -> float -> int = fun f t -> failwith "JS"

[<Emit("window.clearTimeout($0)")>]
let clearTimeout : int -> unit = fun id -> failwith "JS"



