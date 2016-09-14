module LayoutMgrImpl

open Util
open CSS
open Panel
open LayoutMgr
open Measure

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

type FlexLayoutMgr(flexDirection : FlexDirection) =
  interface LayoutMgr<Panel, RenderMsg> with
    member self.childStyles idx panel =
      [ ("display", "flex") ]
    member self.parentStyles panel =
      [ ("display", "flex") ;
        ("flex-direction", stringOfFlexDirection flexDirection)
      ]
    member self.update msg = self :> LayoutMgr<Panel, RenderMsg>
    member self.serialize panel =
      SerializeData.map 
        [ ("type", SerializeData.string "FlexLayoutMgr") ;
          ("flex-direction", 
           SerializeData.string (stringOfFlexDirection flexDirection))
        ]
