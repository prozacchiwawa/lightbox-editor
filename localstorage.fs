module LocalStorage

open Fable.Core

open Q
open Storage

[<Emit("return new window.qpromise.promise(window.localStorage.getItem($0))")>]
let localStorageGet_ : string -> string Q.Promise = fun _ -> failwith "JS"

[<Emit("window.localStorage.setItem($0,$1);")>]
let localStorageSet_ : string -> string -> unit = fun _ _ -> failwith "JS"

type LocalStorage() =
  interface Storage.Storage with
    member self.get name = 
      localStorageGet_ name
    member self.set name value =
      Q.promise (localStorageSet_ name value)
