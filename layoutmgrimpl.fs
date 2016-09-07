module LayoutMgrImpl

open Util
open CSS
open Panel
open LayoutMgr
open Measure

type Panel = Panel.Panel
type RenderMsg = Measure.RenderMsg

let innerLayoutView 
      (self : LayoutMgr<Panel, RenderMsg>) 
      styles getLayoutMgr 
      (renderPanel : (string * string) list -> RenderMsg list -> Panel -> Panel -> RenderMsg)
      (parent : Panel)
      (panel : Panel)  =
  let renderChild parent i p =
    let layoutMgr : LayoutMgr<Panel, RenderMsg> = getLayoutMgr p in
    let styles = self.childStyles i p in
    layoutMgr.view styles getLayoutMgr renderPanel parent p
  in
  let children = List.mapi (renderChild panel) panel.children in
  let styles = self.parentStyles panel in
  renderPanel styles children parent panel
                
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
    member self.view styles getLayoutMgr renderPanel parent panel =
      innerLayoutView self styles getLayoutMgr renderPanel parent panel
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
