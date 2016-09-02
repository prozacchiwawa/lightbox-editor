module Zip

open Fable.Core
open Fable.Import.Browser

open Q
open Blob

type Blob = Blob.Blob

type Zip = Unused0
type Dir = Unused1

[<Emit("window.jszip.createZip()")>]
let createZip : unit -> Zip = fun _ -> failwith "JS"

[<Emit("window.jszip.readZip($0)")>]
let readZip : Blob -> Zip Q.Promise = fun _ -> failwith "JS"

[<Emit("window.jszip.createZipFile($0,$1,$2)")>]
let createZipFile : Zip -> string -> Blob -> Zip Q.Promise = fun _ _ _ -> failwith "JS"

[<Emit("window.jszip.createZipDir($0,$1)")>]
let createZipDir : Zip -> string -> Dir Q.Promise = fun _ _ -> failwith "JS"

[<Emit("window.jszip.createZipDirFile($0,$1,$2)")>]
let createZipDirFile : Dir -> string -> Blob -> Dir Q.Promise = fun _ _ _ -> failwith "JS"

[<Emit("window.jszip.remoteZipFile($0,$1)")>]
let removeZipFile : Zip -> string -> Zip Q.Promise = fun _ _ -> failwith "JS"

[<Emit("window.jszip.zipGenerate($0)")>]
let zipGenerate : Zip -> Blob Q.Promise = fun _ -> failwith "JS"

[<Emit("window.jszip.zipContentFileString($1,$0)")>]
let zipContentFileString : string -> Zip -> string Q.Promise = fun _ _ -> failwith "JS"
