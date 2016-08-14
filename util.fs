module Util

open Fable.Core

[<Emit("console.log($0,$1)")>]
let log : string -> 'a -> unit = fun s o -> failwith "JS only"

[<Emit("('' + $0)")>]
let toString : 'a -> string = fun a -> failwith "JS only"

[<Emit("Math.min($0,$1)")>]
let minPrim : float -> float -> float = fun a b -> failwith "JS only"

[<Emit("Math.max($0,$1)")>]
let maxPrim : float -> float -> float = fun a b -> failwith "JS only"

let min d l =
  match l with
  | [] -> d
  | hd :: tl -> List.fold (fun s a -> minPrim s a) hd tl
                          
let max d l =
  match l with
  | [] -> d
  | hd :: tl -> List.fold (fun s a -> maxPrim s a) hd tl
