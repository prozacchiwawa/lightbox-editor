module Input

open Util
open Html

type Msg =
  | NoOp
  | InputValueChanged of string * string

type EditorInstance =
  {
    currentValue : string ;
    good : string -> bool ;
    renderClass : (string * string) ;
  }

type EditorSet =
  {
    editors : Map<string, EditorInstance> ;
  }

let init _ =
  { editors = new Map<string, EditorInstance>([]) }

let create name editor editors =
  { editors with editors = Map.add name editor editors.editors }

let update action editors =
  match action with
  | InputValueChanged (name,value) ->
     if Map.containsKey name editors.editors then
       let editor = Map.find name editors.editors in
       { editors with 
         editors = 
           Map.add name { editor with currentValue = value } editors.editors
       }
     else
       editors
  | _ -> editors

let view html name editor =
  let classOfEditor =
    match (editor.good editor.currentValue, editor.renderClass) with
    | (true, (className, _)) -> className
    | (false, (_, className)) -> className
  in
  html.div
    [html.className classOfEditor]
    []
    [
      html.input 
        [html.inputValue editor.currentValue]
        [Html.onInput html (fun evt -> InputValueChanged (name, evt.target.value))]
        []
    ]

let map f editors =
  editors.editors |> Map.toList |> List.map (fun (n,v) -> f n v)
