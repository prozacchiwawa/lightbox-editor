module Serialize

open Fable.Core

open SerializeData

type Json = Unused0  
type Constructors =
  {
    jsnull : unit -> Subkey ;
    string : string -> Subkey ;
    float : float -> Subkey ;
    bool : bool -> Subkey ;
    list : Json array -> Subkey ;
    dict : Json -> Subkey
  }

[<Emit("window.objinterface.keysOf($0)")>]
let keysOf : Json -> string array = fun j -> failwith "JS"

[<Emit("window.objinterface.getKey($0,$1)")>]
let getKey : string -> Json -> Json option = fun s j -> failwith "JS"

[<Emit("window.objinterface.jsonToSubkey($0,$1)")>]
let jsonToSubkey_ : Constructors -> Json -> Subkey = fun constructors json -> failwith "JS"

let rec jsonToSubkey v =
  jsonToSubkey_ 
    {
      jsnull = fun _ -> Null ;
      string = fun s -> String s ;
      float = fun f -> Float f ;
      bool = fun b -> Bool b ;
      list = fun a -> 
             List 
               (a 
                |> Array.toList 
                |> List.map jsonToSubkey) ;
      dict = 
        fun j ->
          j 
          |> (fun m -> keysOf m)
          |> Array.toList 
          |> List.map 
               (fun k -> 
                 match getKey k j with 
                 | None -> [] 
                 | Some v -> [k, jsonToSubkey v])
          |> List.concat
          |> Map<string, Subkey>
          |> (fun m -> Dict m)
    }
    v

[<Emit("{ try { return JSON.parse($1); } catch(e) { return ($0)(''+e); } }")>]
let parse : (string -> Json) -> string -> Json = fun s e -> failwith "JS"

[<Emit("JSON.stringify($0)")>]
let stringify : Json -> string = fun s -> failwith "JS"

[<Emit("null")>]
let jsonNull : unit -> Json = fun s -> failwith "JS"

[<Emit("$0")>]
let jsonString : string -> Json = fun s -> failwith "JS"

[<Emit("$0")>]
let jsonFloat : float -> Json = fun f -> failwith "JS"

[<Emit("$0")>]
let jsonBool : bool -> Json = fun b -> failwith "JS"

[<Emit("$0")>]
let jsonArray : Json array -> Json = fun a -> failwith "JS"

[<Emit("window.objinterface.jsonMap($0)")>]
let jsonMap : (string * Json) array -> Json = fun a -> failwith "JS"

let rec subkeyToJson sk =
  match sk with
  | Null -> jsonNull ()
  | String s -> jsonString s
  | Float f -> jsonFloat f
  | Bool b -> jsonBool b
  | List a -> a |> List.map subkeyToJson |> List.toArray |> (fun a -> jsonArray a)
  | Dict m -> 
     m 
     |> Map.toSeq 
     |> Seq.toList 
     |> List.map (fun (k,v) -> (k,subkeyToJson v)) 
     |> List.toArray
     |> (fun m -> jsonMap m)
