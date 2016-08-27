module LayoutMgrImpl

open Util
open CSS
open Panel
open LayoutMgr

let innerLayoutView (self : LayoutMgr<Panel, VDom.VNode>) styles getLayoutMgr renderPanel panel =
  let renderChild i p =
    let layoutMgr : LayoutMgr<Panel, VDom.VNode> = getLayoutMgr p in
    let styles = self.childStyles i p in
    layoutMgr.view styles getLayoutMgr renderPanel p
  in
  let children = List.mapi renderChild panel.children in
  let styles = self.parentStyles panel in
  renderPanel styles children panel
                
type FreeLayoutMgr() =
  interface LayoutMgr<Panel, VDom.VNode> with
    member self.view styles getLayoutMgr renderPanel panel =
      innerLayoutView self styles getLayoutMgr renderPanel panel
    member self.childStyles idx panel = []
    member self.parentStyles panel =
      let draggerUL = upperLeft panel in
      let draggerBR = lowerRight panel in
      [
        ("position", Panel.positionString panel.position);
        ("left", CSS.pixelPos draggerUL.x);
        ("top", CSS.pixelPos draggerUL.y);
        ("width", CSS.pixelPos (draggerBR.x - draggerUL.x));
        ("height", CSS.pixelPos (draggerBR.y - draggerUL.y))
      ]
    member self.update msg = self :> LayoutMgr<Panel, VDom.VNode>
