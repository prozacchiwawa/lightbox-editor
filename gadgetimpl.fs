module GadgetImpl

open Util
open CSS
open Html
open Panel
open Gadget
open Measure
open Input

type Panel = Panel.Panel
type RenderMsg = Measure.RenderMsg

type FlexDirection =
  | FlexColumn
  | FlexRow

let stringOfFlexDirection fd =
  match fd with
  | FlexColumn -> "column"
  | FlexRow -> "row"

let flexDirectionOfString fds =
  match fds with
  | "row" -> FlexRow
  | _ -> FlexColumn

let layoutMgrView editors (html : Gadget.Msg Html) panel =
  html.div
    []
    []
    (Input.map 
       (fun n ed -> 
         html.div 
           [html.className "control-row-gang"] []
           [
             html.div
               [html.className "control-row-label"] []
               [
                 html.text n ;
                 ed.view (Html.map (fun m -> InputMsg m) html) n
               ]
           ]
       ) 
       editors
    )

let layoutMgrInputUpdate m editors' lm f =
  let updatedEditors = Input.update m editors' in
    List.fold
      (fun lm ((name,editor) : (string * EditorInstance)) ->
        let cv = editor.currentValue () in
        f lm updatedEditors name cv
      )
      lm
      (Input.map Util.tuple2 updatedEditors)

let layoutMgrUpdate msg editors lm upd =
  match msg with
  | InputMsg m ->
     let result = 
       layoutMgrInputUpdate m editors lm upd
     in
     result :> Gadget<Panel, RenderMsg>
      | _ -> lm :> Gadget<Panel, RenderMsg>

let createSelector name value vlist =
  new InputSelector(
        value,
        (Set<string> vlist)
      )

let createTextEditor name value =
  new InputEditor(
        value,
        (fun value -> true),
        ("numeric-input-good", "numeric-input-good")
      )

type FlexGadget(flexDirection : FlexDirection, editors' : EditorSet option) =
  let editors = 
    editors'
    |> Util.maybeWithDefault
         (Input.init ()
          |> Input.create "flex-direction"
                          (createSelector 
                             "flex-direction" 
                             (stringOfFlexDirection flexDirection) 
                             ["row" ; "column"]
                          )
         )
  in
  interface Gadget<Panel, RenderMsg> with
    member self.name () = "Flex Layout"
    member self.childStyles idx panel =
      [ ("display", "flex") ]
    member self.parentStyles panel =
      [ ("display", "flex") ;
        ("flex-direction", stringOfFlexDirection flexDirection)
      ]
    member self.modify panel = panel
    member self.view html panel = 
      layoutMgrView editors html panel
    member self.update msg = 
      layoutMgrUpdate msg editors self
         (fun lm e n v ->
           match n with
           | "flex-direction" -> 
              new FlexGadget(flexDirectionOfString v, Some e)
           | _ -> self
         )
    member self.serialize panel =
      SerializeData.map 
        [ ("type", SerializeData.string "FlexGadget") ;
          ("flex-direction", 
           SerializeData.string (stringOfFlexDirection flexDirection))
        ]

type TextGadget(text : string, editors' : EditorSet option) =
  let editors = 
    editors'
    |> Util.maybeWithDefault
         (Input.init ()
          |> Input.create "text" (createTextEditor "text" text)
         )
  in
  interface Gadget<Panel, RenderMsg> with
    member self.name () = "Text"
    member self.childStyles idx panel = []
    member self.parentStyles panel = []
    member self.modify panel = { panel with text = text }
    member self.view html panel = 
      layoutMgrView editors html panel
    member self.update msg =
      layoutMgrUpdate msg editors self
         (fun lm e n v ->
           match n with
           | "text" -> 
              new TextGadget(text, Some e)
           | _ -> self
         )
    member self.serialize panel =
      SerializeData.map 
        [ ("type", SerializeData.string "TextGadget") ;
          ("text", SerializeData.string text)
        ]
