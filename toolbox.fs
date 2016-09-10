module Toolbox

(* A toolbox is an element that contains a gallery of draggable objects.
 * dragging an object from the toolbox *)

open Point
open Panel

type Msg =
  | NoOp

type ToolId =
  | TextChildTool

type Tool =
  {
    id : ToolId ;
    render : Msg Html.Html -> Tool -> VDom.VNode ;
    apply : Tool -> Panel -> Panel ;
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
        { id = TextChildTool ; 
          render = fontAwesomeRender "fa-newspaper-o" ;
          apply = 
            fun tool panel ->
            let newPanel =
              {
                id = Util.genId() ;
                text = "Lorem ipsum dolor sit amet" ;
                background = "" ;
                children = [] ;
                layout = LayoutMgrImpl.FlexLayoutMgr(LayoutMgrImpl.FlexRow)
              }
            in
            { panel with children = List.concat [panel.children;[newPanel]] }
        }
      ]
  }

let itemFromId tid toolbox =
  toolbox.tools
  |> List.filter (fun t -> t.id = tid)

let itemFromCoords (at : Point) toolbox =
  let n = Util.ifloor ((at.y - 5.0) / 105.0) in
  Util.listNth None (fun a -> Some a) toolbox.tools n

let viewDragging (html : Msg Html.Html) tid toolbox =
  toolbox
  |> itemFromId tid
  |> List.map
       (fun t -> 
         html.div [html.className "toolbox-tool"] [] [t.render html t])

let viewTool (html : Msg Html.Html) t =
  html.div
    [html.className "toolbox-tool"] []
    [t.render html t]

let view (html : Msg Html.Html) tb =
  html.div
    [html.className "toolbox"] [] 
    (List.map (viewTool html) tb.tools)

let update msg tb =
  tb

let applyTool toolbox tid panel =
  toolbox.tools
  |> List.filter (fun t -> t.id = tid)
  |> List.map (fun t -> t.apply t panel)
  |> Util.headWithDefault panel
