module Blob

open Fable.Core
open Fable.Import.Browser

open Q

type BlobInput = 
  | StringData of string
  | Base64Data of string

type Blob = Unused0

[<Emit("window.blobinterface.create($0,$1)")>]
let create : string -> BlobInput list -> Blob = fun _ _ -> failwith "JS"

[<Emit("{ console.log($0); return window.blobinterface.toBase64($0) }")>]
let toBase64 : Blob -> string Q.Promise = fun _ -> failwith "JS"
