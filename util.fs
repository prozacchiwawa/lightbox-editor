module Util

open Fable.Core

[<Emit("btoa($0)")>]
let btoa : string -> string = fun s -> failwith "JS"

[<Emit("atob($0)")>]
let atob : string -> string = fun s -> failwith "JS"

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

let rec listNth d f l n =
  match expose "listNth" (d,l,n) with
  | (_,hd :: tl,0) -> f hd
  | (d,[],_) -> d
  | (_,hd :: tl,n) -> listNth d f tl (n - 1)

let headWithDefault d l =
  match l with 
  | [] -> d
  | hd :: _ -> hd

let maybeWithDefault d m =
  match m with
  | None -> d
  | Some v -> v

let flip f b a = f a b

let andMap f l = l |> List.map f |> List.concat

let maybeMap f m =
  match m with
  | None -> None
  | Some a -> Some (f a)

let tuple2 a b = (a,b)

[<Emit("Math.floor($0)")>]
let floor : float -> float = fun a -> failwith "JS only"

[<Emit("Math.floor($0)")>]
let ifloor : float -> int = fun a -> failwith "JS only"

[<Emit("Math.floor($0 + 0.5)")>]
let round : float -> float = fun a -> failwith "JS only"

let snap (a : float) (b : float) = (round (a / b)) * b

let foldInto f list init = List.fold f init list

[<Emit("{ var a = $0; if (!a.selected) { debugger; } return a; }")>]
let stop : 'a -> 'a = fun a -> failwith "JS only"
