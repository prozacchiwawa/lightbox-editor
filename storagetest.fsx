#r "node_modules/fable-core/Fable.Core.dll"

open Fable.Core
open Fable.Import.Browser

#load "q.fs"
#load "storage.fs"
#load "localstorage.fs"

open Q
open Storage
open LocalStorage

[<Emit("console.log($0)")>]
let log : string -> unit = fun _ -> failwith "JS"

let main _ =
  let ls = (new LocalStorage.LocalStorage()) :> Storage.Storage in
  ls.set "hi" "there"
  |>> (fun _ -> ls.get "hi")
  ||> (fun s -> log s)
  ||| (fun e -> log (String.concat ":" ["error";e]))
