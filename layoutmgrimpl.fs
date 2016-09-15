module LayoutMgrImpl

open Util
open CSS
open Panel
open LayoutMgr
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

let createSelector name value vlist =
  new InputSelector(
        value,
        (Set<string> vlist)
      )

type FlexLayoutMgr(flexDirection : FlexDirection) =
  let editors = 
    Input.init ()
    |> Input.create "flex-direction"
         (createSelector 
            "flex-direction" 
            (stringOfFlexDirection flexDirection) 
            ["row" ; "column"]
         )
  in
  interface LayoutMgr<Panel, RenderMsg> with
    member self.childStyles idx panel =
      [ ("display", "flex") ]
    member self.parentStyles panel =
      [ ("display", "flex") ;
        ("flex-direction", stringOfFlexDirection flexDirection)
      ]
    member self.view html panel = 
      html.div
        []
        []
        (Input.map (fun n ed -> ed.view (Html.map (fun m -> InputMsg m) html) n) editors)
    member self.update msg = self :> LayoutMgr<Panel, RenderMsg>
    member self.serialize panel =
      SerializeData.map 
        [ ("type", SerializeData.string "FlexLayoutMgr") ;
          ("flex-direction", 
           SerializeData.string (stringOfFlexDirection flexDirection))
        ]
