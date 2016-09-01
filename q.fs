module Q

open Fable.Core

type 'a Promise = Unused0 of 'a

[<Emit("window.qpromise.promise($0)")>]
let promise : 'a -> 'a Promise = fun a -> failwith "JS"

[<Emit("window.qpromise.complete($0,$1)")>]
let map : 'a Promise -> ('a -> 'b) -> 'b Promise = fun p f -> failwith "JS"

[<Emit("window.qpromise.complete($0,$1)")>]
let andThen : 'a Promise -> ('a -> 'b Promise) -> 'b Promise = fun p f -> failwith "JS"

[<Emit("window.qpromise.error($0,$1)")>]
let error : 'a Promise -> (string -> 'b) -> 'b Promise = fun p f -> failwith "JS"

let (|>>) promise f =
  andThen promise f

let (||>) promise f =
  map promise f

let (|||) promise f =
  error promise f
