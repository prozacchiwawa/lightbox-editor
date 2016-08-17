module Input

open Util
open VDom
open Html

type Msg =
  | NoOp
  | InputValueChanged of string * string

type EditorInstance =
  abstract member currentValue : unit -> string
  abstract member update : string -> EditorInstance
  abstract member good : string -> bool
  abstract member view : Msg Html -> string -> VDom.VNode

type InputEditor(currentValue : string, good : string -> bool, renderClass : string * string) =
  interface EditorInstance with
    member self.currentValue () = currentValue
    member self.update sv = new InputEditor(sv, good, renderClass) :> EditorInstance
    member self.good str = good str
    member self.view html name =
      let classOfEditor =
        match (good currentValue, renderClass) with
        | (true, (className, _)) -> className
        | (false, (_, className)) -> className
      in
      html.div
        [html.className classOfEditor]
        []
        [
          html.input 
            [html.inputValue currentValue]
            [Html.onInput html (fun evt -> InputValueChanged (name, evt.target.value))]
            []
        ]
  
type InputSelector(currentValue : string, valueList : Set<string>) =
  interface EditorInstance with
    member self.currentValue () = currentValue
    member self.update sv = new InputSelector(sv, valueList) :> EditorInstance
    member self.good str = Set.contains str valueList
    member self.view html name =
      html.div
        [html.className "selector-container"]
        []
        [
          html.select
            [] []
            (valueList |> 
               Set.toList |> 
               List.map 
                 (fun v -> html.option (if currentValue = v then [html.attribute "selected" "true"] else []) [] [html.text v]))
        ]

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
           Map.add name (editor.update value) editors.editors
       }
     else
       editors
  | _ -> editors

let map f editors =
  editors.editors |> Map.toList |> List.map (fun (n,v) -> f n v)
