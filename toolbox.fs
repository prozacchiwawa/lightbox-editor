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
    included : Map<string, Gadget<Panel,RenderMsg> > ;
    original : Map<string, Gadget<Panel,RenderMsg> >
  }

let fontAwesomeRender icon (html : Msg Html.Html) t =
  html.i 
    [ html.className (String.concat " " ["fa"; icon]) ; 
      html.attribute "aria-hidden" "true"] 
    [] []

let create (tools : Gadget<Panel,RenderMsg> list) =
  let included = 
    new Map<string, Gadget<Panel, RenderMsg> >
      (List.map (fun (t : Gadget<Panel,RenderMsg>) -> (t.name (), t)) tools)
  in
  { 
    included = included ;
    original = included ;
    tools = 
      [
        { render = FontAwesome "fa-wrench" ;
          gadget = new FlexGadget(FlexRow, None)
        } ;
        { render = FontAwesome "fa-newspaper-o" ;
          gadget = new TextGadget("Lorem ipsum dolor sit amet", None)
        } ;
        { render = FontAwesome "fa-columns" ;
          gadget = new SidebarGadget(100.0, None)
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
    if Map.containsKey (t.gadget.name ()) toolbox.included then
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
     if Map.containsKey g tb.included then
       { tb with included = Map.remove g tb.included }
     else
       { tb with 
         included = 
           List.concat 
             [
               tb.original
               |> Map.tryFind g
               |> Util.maybeSingleton ;
               tb.tools
               |> List.map (fun t -> t.gadget)
               |> List.filter (fun gadget -> (gadget.name ()) = g)
             ]
           |> List.map 
                (fun g -> Map.add (g.name ()) g tb.included)
           |> Util.headWithDefault tb.included
       }
  | _ -> tb

