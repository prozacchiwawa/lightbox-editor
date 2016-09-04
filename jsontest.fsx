#r "node_modules/fable-core/Fable.Core.dll"

open Fable.Core
open Fable.Import.Browser

#load "util.fs"
#load "serialize.fs"

open Util
open Serialize

let log = Util.expose

let tests =
  [
    jsnull () ;
    string "foo" ;
    num 1.9 ;
    bool false ;
    list [ string "a" ; string "b" ; string "c" ] ;
    [ ("a", num 1.0) ; ("b", num 2.0) ; ("c", list [bool true; bool false]) ] |> Map<string, Subkey> |> dict
  ]

let testValue kn =
  let n = subkeyToJson kn in
  let ns = stringify n in
  let np = parse ns in
  let nk = jsonToSubkey np in
  log "subkey" kn ;
  log "value" n ;
  log "stringify" ns ;
  log "parsed" np ;
  log "read" nk ;
  kn = nk

let main _ =
  log "Tests" (List.map testValue tests)
