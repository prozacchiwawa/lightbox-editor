module Toolbox

(* A toolbox is an element that contains a gallery of draggable objects.
 * dragging an object from the toolbox *)

open DomUnits
open Point
open Panel
open Gadget
open Measure
open GadgetImpl

type Msg =
  | NoOp
  | ToggleGadget of string
  | SelectGadgets of Set<string>

type ToolRender =
  FontAwesome of string

type Tool =
  {
    render : ToolRender ;
    gadget : Gadget<Panel,RenderMsg>
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
        { render = FontAwesome "fa-columns" ;
          gadget = new FlexGadget(FlexRow, None)
        }
        { render = FontAwesome "fa-newspaper-o" ;
          gadget = new TextGadget("Lorem ipsum dolor sit amet", None)
        }
      ]
  }

let itemFromCoords (at : Point) toolbox =
  let n = Util.ifloor ((at.y - 5.0) / 105.0) in
  if at.x < 110.0 then
    Util.listNth None (fun a -> Some a) toolbox.tools n
  else
    None

let viewTool (html : Msg Html.Html) toolbox t =
  let rendered =
    match t.render with
    | FontAwesome icon ->
       fontAwesomeRender icon html t
  in
  let toolClass =
    if Set.contains (t.gadget.name ()) toolbox.included then
      "toolbox-tool toolbox-tool-selected"
    else
      "toolbox-tool"
  in
  html.div 
    [html.className toolClass]
    [Html.onMouseClick html (fun m -> ToggleGadget (t.gadget.name ()))]
    [ rendered ; 
      html.div
        [ html.className "toolbox-label" ]
        []
        [ html.text (t.gadget.name ()) ]
    ]

let view (html : Msg Html.Html) tb =
  html.div
    [html.className "toolbox"] []
    (List.map (viewTool html tb) tb.tools)

let update msg tb =
  match msg with
  | ToggleGadget g ->
     if Set.contains g tb.included then
       { tb with included = Set.remove g tb.included }
     else
       { tb with included = Set.add g tb.included }
  | _ -> tb

