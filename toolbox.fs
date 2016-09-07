module Toolbox

(* A toolbox is an element that contains a gallery of draggable objects.
 * dragging an object from the toolbox *)

open Point

type Msg =
  | NoOp

type ToolId =
  | TextChildTool

type Tool =
  {
    id : ToolId ;
    render : Msg Html.Html -> Tool -> VDom.VNode
  }

type State =
  {
    tools : Tool list 
  }

let fontAwesomeRender icon (html : Msg Html.Html) t =
  html.i 
    [ html.className (String.concat " " ["fa"; icon]) ; 
      html.attribute "aria-hidden" "true"] 
    [] []

let create _ =
  { 
    tools = 
      [
        { id = TextChildTool ; render = fontAwesomeRender "fa-newspaper-o" }
      ]
  }

let itemFromCoords (at : Point) toolbox =
  let n = Util.ifloor ((at.y - 5.0) / 105.0) in
  Util.listNth None (fun a -> Some a) toolbox.tools n

let viewTool (html : Msg Html.Html) t =
  html.div
    [html.className "toolbox-tool"] []
    [t.render html t]

let view (html : Msg Html.Html) tb =
  html.div
    [html.className "toolbox"] [] 
    (List.map (fun t -> viewTool html t) tb.tools)

let update msg tb =
  tb
