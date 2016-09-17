module Toolbox

(* A toolbox is an element that contains a gallery of draggable objects.
 * dragging an object from the toolbox *)

open DomUnits
open Point
open Panel
open Gadget
open Measure

type Msg =
  | NoOp
  | SelectGadgets of Set<string>

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
  }

type State =
  {
    tools : Tool list ;
    included : Set<string> ;
    original : Set<string>
  }

let fontAwesomeRender icon (html : Msg Html.Html) t =
  html.i 
    [ html.className (String.concat " " ["fa"; icon]) ; 
      html.attribute "aria-hidden" "true"] 
    [] []

let create (tools : Gadget<Panel,RenderMsg> list) =
  let included =
    tools
    |> List.map (fun t -> t.name ())
    |> Set<string>
  in
  { 
    included = included ;
    original = included ;
    tools = 
      [
        { id = EmptyChildListTool ;
          render = FontAwesome "fa-columns" ;
        } ;
        { id = EmptyChildRowTool ;
          render = FontAwesome "fa-list" ;
        } ;
        { id = TextChildTool ; 
          render = FontAwesome "fa-newspaper-o" ;
        }
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

