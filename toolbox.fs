module Toolbox

(* A toolbox is an element that contains a gallery of draggable objects.
 * dragging an object from the toolbox *)

open DomUnits
open Point
open Panel

type Msg =
  | NoOp

type ToolId =
  | EmptyChildListTool
  | EmptyChildRowTool
  | TextChildTool

type ToolRender =
  FontAwesome of string

type Tool =
  {
    id : ToolId ;
    render : ToolRender
    apply : Tool -> Panel -> Panel -> Panel ;
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
  let newPanel =
    {
      id = "" ;
      text = "" ;
      background = "" ;
      children = [] ;
      dummyChildren = [ Panel.dummy ] ;
      useWidth = Unspecified ;
      width = 0.0 ;
      useHeight = Unspecified ;
      height = 0.0 ;
      layout = []
    }
  in
  { 
    tools = 
      [
        { id = EmptyChildListTool ;
          render = FontAwesome "fa-columns" ;
          apply = 
            fun tool panel root ->
            let p =
              { newPanel with
                id = Util.genId() ;
                layout =
                  [
                    LayoutMgrImpl.FlexLayoutMgr(LayoutMgrImpl.FlexColumn)
                  ]
              }
            in
            { panel with children = List.concat [panel.children;[p]] }
        } ;
        { id = EmptyChildRowTool ;
          render = FontAwesome "fa-list" ;
          apply = 
            fun tool panel root -> 
            let p = 
              { newPanel with
                id = Util.genId() ;
                layout =
                  [
                    LayoutMgrImpl.FlexLayoutMgr(LayoutMgrImpl.FlexRow)
                  ]
              }
            in
            { panel with children = List.concat [panel.children;[p]] }
        } ;
        { id = TextChildTool ; 
          render = FontAwesome "fa-newspaper-o" ;
          apply = 
            fun tool panel root ->
            { panel with 
              id = Util.genId() ;
              text = "Lorem ipsum dolor sit amet" ;
              children = List.concat [panel.children;[newPanel]] 
            }
        } ;
      ]
  }

let itemFromId tid toolbox =
  toolbox.tools
  |> List.filter (fun t -> t.id = tid)

let itemFromCoords (at : Point) toolbox =
  let n = Util.ifloor ((at.y - 5.0) / 105.0) in
  if at.x < 110.0 then
    Util.listNth None (fun a -> Some a) toolbox.tools n
  else
    None

let viewTool (html : Msg Html.Html) t =
  let rendered =
    match t.render with
    | FontAwesome icon ->
       fontAwesomeRender icon html t
  in
  html.div [html.className "toolbox-tool"] [] [ rendered ]

let viewDragging (html : Msg Html.Html) tid toolbox =
  toolbox
  |> itemFromId tid
  |> List.map (viewTool html)

let view (html : Msg Html.Html) tb =
  html.div
    [html.className "toolbox"] [] 
    (List.map (viewTool html) tb.tools)

let update msg tb =
  tb

let applyTool toolbox tid panel root =
  toolbox.tools
  |> List.filter (fun t -> t.id = tid)
  |> List.map (fun t -> Panel.replace (t.apply t panel root) root)
  |> Util.headWithDefault panel
