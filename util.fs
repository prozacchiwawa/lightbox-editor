module Util

open Fable.Core

[<Emit("{ console.log($0,$1); return $1; }")>]
let log : string -> 'a -> 'a = fun s o -> failwith "JS only"

[<Emit("{ console.log($0,JSON.stringify($1)); return $1; }")>]
let expose : string -> 'a -> 'a = fun s o -> failwith "JS only"

[<Emit("('' + $0)")>]
let toString : 'a -> string = fun a -> failwith "JS only"

[<Emit("Math.min($0,$1)")>]
let minPrim : float -> float -> float = fun a b -> failwith "JS only"

[<Emit("Math.max($0,$1)")>]
let maxPrim : float -> float -> float = fun a b -> failwith "JS only"

[<Emit("window.generateId()")>]
let genId : unit -> string = fun _ -> failwith "JS only"

let min d l =
  match l with
  | [] -> d
  | hd :: tl -> List.fold (fun s a -> minPrim s a) hd tl
                          
let max d l =
  match l with
  | [] -> d
  | hd :: tl -> List.fold (fun s a -> maxPrim s a) hd tl

[<Emit("Math.abs($0)")>]
let abs : float -> float = fun a -> failwith "JS only"

let between a b c =
  let interval = abs (b - a) in
  (abs (c - a)) < interval && (abs (c - b)) < interval

[<Emit("isNaN($0)")>]
let isNaN : float -> bool = fun f -> failwith "JS only"

[<Emit("window.parseFloat($0)")>]
let parseFloat_ : string -> float = fun f -> failwith "JS only"

let parseFloat str =
  let flt = parseFloat_ str in
  if isNaN flt then None else Some flt
