#r "node_modules/fable-core/Fable.Core.dll"
   
open Fable.Core
open Fable.Import.Browser

#load "util.fs"
#load "domunits.fs"
#load "dom.fs"
#load "point.fs"
#load "grid.fs"
#load "vdom.fs"
#load "html.fs"
#load "serializedata.fs"
#load "css.fs"
#load "gridrenderer.fs"
#load "measure.fs"
#load "input.fs"
#load "gadget.fs"
#load "panel.fs"
#load "gadgetimpl.fs"
#load "measurerender.fs"
#load "wireframe.fs"
#load "controls.fs"
#load "q.fs"
#load "storage.fs"
#load "localstorage.fs"
#load "timer.fs"
#load "serialize.fs"
#load "gadgetload.fs"
#load "panelserialize.fs"
#load "toolbox.fs"
#load "dragcontroller.fs"

open Q
open DomUnits
open Measure
open Gadget

type Point = Point.Point
type Panel = Panel.Panel
type UI = Controls.UI
type Grid = Grid.Grid
type MeasureMsg = Measure.MeasureMsg

type Color = { r : int ; b : int ; g : int ; a : int }
               
type DragTarget =
  | Panel of string

type Msg = 
  | NoOp 
  | AddChild of string
  | RemovePanel of string
  | MouseDown of float * float
  | MouseUp of float * float
  | MouseMove of float * float
  | ControlMsg of Controls.Msg
  | Measures of Measure.MeasureMsg
  | Inactivity
  | Save
  | LoadAutosave
  | AutosaveLoaded of SerializeData.Subkey
  | Rerender
  | ToolboxMsg of Toolbox.Msg
  | DragStart of (Point * DragTarget)
  | DragMove of (Point * Point * DragTarget option * DragTarget)
  | DragEnd of (Point * Point * DragTarget option * DragTarget)
  | DraggerClick of (Point * DragTarget)
  | DismissToolbox of (Map<string, Gadget<Panel,RenderMsg> >)

type DragViz =
  {
    start : Point ;
    endAt : Point ;
    subject : DragTarget ;
    target : DragTarget option
  }

type State = 
  { 
    palette : Map<string, Color> ;
    root : Panel ;
    dirtyPanels : bool ;
    selected : Panel ;
    backgroundUrl : string ;
    opacity : float ;
    dragger : (DragTarget, State, Msg) DragController.State ;
    grid : Grid ;
    ui : UI ;
    measure : MeasureMsg ;
    inactivityTimerId : int ;
    localStorage : Storage.Storage ;
    toolbox : Toolbox.State option ;
    dragViz : DragViz option ;
  }

let findDraggableSubject (pt : Point) state =
  let rdim : TextRectangle = 
    DOM.getBoundingClientRect "canvas-frame" in
  state.measure.data 
  |> Measure.fromCoord (Point.ctor (pt.x - rdim.left) (pt.y - rdim.top))
  |> List.map (fun i -> Some (Panel i.key))
  |> Util.headWithDefault None
                            
let init arg =
  let root = 
    { 
      Panel.id = "root" ;
      Panel.text = "" ;
      Panel.children = [] ;
      Panel.layout = []
    }
  in
  let grid = Grid.create false (Point.ctor 0. 0.) (Point.ctor 16. 16.) in
  { 
    palette = Map<string, Color> [] ;
    selected = root ;
    backgroundUrl = "" ;
    opacity = 0.4 ;
    root = root ;
    dirtyPanels = false ; 
    dragger = 
      { DragController.emptyDragController NoOp with
        getSubject = (fun pt state -> findDraggableSubject pt state) ;
        getTarget = (fun pt state -> findDraggableSubject pt state) ;
        isSameObject = (fun state a b -> a = b) ;
        authorPickupMsg = fun st state sub -> DragStart (st,sub) ;
        authorMoveMsg = fun st pt state tgt sub -> DragMove (st,pt,tgt,sub) ;
        authorDropMsg = fun st pt state tgt sub -> DragEnd (st,pt,tgt,sub) ;
        authorClickMsg = fun st state sub -> DraggerClick (st,sub)
      }
    grid = grid ;
    ui = Controls.init grid root ;
    measure = Measure.emptyMeasure ;
    inactivityTimerId = -1 ;
    localStorage = new LocalStorage.LocalStorage() :> Storage.Storage ;
    toolbox = None ;
    dragViz = None ;
  }

let save state =
  [ ("backgroundUrl", SerializeData.string state.backgroundUrl) ;
    ("opacity", SerializeData.num state.opacity) ;
    ("root", PanelSerialize.save state.root)
  ]
  |> SerializeData.map

let rec update action state =
  let selectPanel (panel : Panel) state =
    match state.toolbox with
    | Some _ -> state
    | None ->
       { state with 
         selected = panel ; 
         ui = 
           Controls.select 
             panel state.root state.ui 
       }
  in
  let handleMouseEvent evt =
    let report = DragController.update evt state state.dragger in
    let newState = { state with dragger = report.dragger } in
    let result = 
      List.fold 
        (fun state msg -> update msg state)
        newState
        report.dispatched
    in
    result
  in
  match Util.expose "action" action with
  | NoOp -> state
  | MouseDown (x,y) ->
     handleMouseEvent (DragController.MouseDown (Point.ctor x y))
  | MouseUp (x,y) ->
     handleMouseEvent DragController.MouseUp
  | MouseMove (x,y) ->
     handleMouseEvent (DragController.MouseMove (Point.ctor x y))
  | DragStart (st,sub) ->
     { state with
       dragViz =
         Some
           {
             start = st;
             endAt = st;
             subject = sub ;
             target = None
           }
     }
  | DragMove (st,pt,tgt,sub) ->
     { state with 
       dragViz = 
         Some 
           { 
             start = st ;
             endAt = pt ;
             subject = sub ;
             target = tgt
           }
     }
  | DragEnd (st,pt,_,_) ->
     { state with dragViz = None }
  | DraggerClick (st,sub) ->
     let stateWithoutViz = { state with dragViz = None } in
     match sub with
       | Panel id ->
          stateWithoutViz.root
          |> Panel.fromId id
          |> List.map (fun panel -> selectPanel panel stateWithoutViz)
          |> Util.headWithDefault stateWithoutViz
       | _ -> stateWithoutViz
  | ControlMsg (Controls.ChangeBackground bg) ->
     { state with backgroundUrl = Util.log "Background" bg }
  | ControlMsg (Controls.SelectPanel pid) ->
     state.root
     |> Panel.fromId pid
     |> Util.headWithDefault state.root
     |> (Util.flip selectPanel) state
  | ControlMsg (Controls.DeletePanel pid) ->
     let reselect = 
       state.root
       |> Panel.parent pid state.root
       |> Util.headWithDefault state.root
       |> (Util.flip selectPanel) state
     in
     { reselect with root = Panel.remove pid state.root ; dirtyPanels = true }
  (* Open the gadgets pane *)
  | ControlMsg (Controls.GadgetPanel pid) -> 
     let s0 = 
       selectPanel 
         (state.root
          |> Panel.fromId pid
          |> Util.headWithDefault state.selected)
         state
     in
     { s0 with
       toolbox = Some (Toolbox.create state.selected.layout)
     }
  | ControlMsg msg ->
     let s0 = { state with ui = Controls.update msg state.ui } in
     let s1 = { s0 with grid = s0.ui.grid } in
     if s1.ui.dirtyPanel then
       let (panel,ui) = Controls.takeUpdate s1.ui in
       { s1 with 
         ui = ui ; 
         root = Panel.replace panel s1.root ; 
         dirtyPanels = true 
       }
     else
       s1
  | ToolboxMsg msg ->
     { state with
       toolbox =
         state.toolbox
         |> Util.maybeMap (fun toolbox -> Toolbox.update msg toolbox)
     }
  | DismissToolbox tools ->
     let panel = 
       { state.selected with
         layout = 
           tools
           |> Map.toList
           |> List.map Util.snd
       }
     in
     { state with 
       toolbox = None ; 
       selected = panel ;
       root = Panel.replace panel state.root
     }
  | _ -> state
         
let cssPixelPos v =
  String.concat "" [Util.toString v; "px"]

let view (html : Msg Html.Html) state =
  let visualizeDrag =
    match state.dragViz with
    | None -> 
       [ html.div [{name = "style"; value = "display: none"}] [] [] ]
    | Some dv ->
       let draggerUL = 
         Point.ctor
           (Util.min 0. [dv.start.x;dv.endAt.x])
           (Util.min 0. [dv.start.y;dv.endAt.y])
       in
       let draggerBR = 
         Point.ctor
           (Util.max 0. [dv.start.x;dv.endAt.x])
           (Util.max 0. [dv.start.y;dv.endAt.y])
       in
       [ html.div
           [ html.className "dragger" ;
             html.style [
                 ("left", cssPixelPos draggerUL.x);
                 ("top", cssPixelPos draggerUL.y);
                 ("width", cssPixelPos (draggerBR.x - draggerUL.x));
                 ("height", cssPixelPos (draggerBR.y - draggerUL.y))
               ]
           ]
           []
           []
       ]
  in
  let mapControlMsg : (Controls.Msg -> Msg) = 
    fun m -> ControlMsg m
  in
  let controlHtml : Controls.Msg Html.Html =
    Html.map mapControlMsg html
  in
  let backgroundSpecification =
    if state.backgroundUrl <> "" then
      [ html.style
          [
            ("background-image", String.concat "" ["url('";state.backgroundUrl;"')"]) ;
            ("opacity", Util.toString state.opacity) ;
            ("background-repeat", "no-repeat") ;
            ("background-position", "center top") ;
            ("background-size", "100% auto") ;
            ("width", "1000px")
          ]
      ]
    else
      []
  in
  let wireframe = 
    Wireframe.view html state.selected.id state.measure
  in
  let dragViz =
    let dragChild = 
      match state.dragViz with
      | None -> [ html.div [] [] [] ]
      | Some dv ->
         match dv.subject with
         | Panel p -> [ html.div [] [] [] ]
    in
    [ html.div [html.className "dragger-container"] [] dragChild ]
  in
  html.div
    [html.className "app-container"]
    [
      Html.onMouseDown 
        html 
        (fun evt -> 
          begin
            VDom.preventDefault evt ;
            MouseDown (evt.pageX, evt.pageY)
          end) ;
      Html.onMouseUp 
        html 
        (fun evt -> 
          begin
            VDom.preventDefault evt ;
            MouseUp (evt.pageX, evt.pageY)
          end) ;
      Html.onMouseMove 
        html 
        (fun evt -> 
          begin
            VDom.preventDefault evt ;
            MouseMove (evt.pageX, evt.pageY)
          end)
    ]
    (List.concat 
       [
         [ 
           html.div
             [html.className "root-container"] []
             [
               html.div
                 [html.className "root"]
                 []
                 [
                   html.iframe 
                     [
                            html.className "canvas-frame";
                            {name = "id"; value = "canvas-frame"};
                            {name = "src"; value = "child.html"}
                     ] [] [] ;
                   html.div 
                     (List.concat
                        [
                          [ html.className "image-overlay" ] ;
                          backgroundSpecification
                        ]
                     ) [] [] ;
                   wireframe
                 ] ;
               html.div
                 [html.className "dragger-container"] []
                 (List.concat
                    [
                      GridRenderer.view html state.grid
                    ]
                 )
             ] ;
         ] ;
         [state.toolbox
          |> Util.maybeMap 
               (fun toolbox ->
                 html.div
                   [html.className "toolbox-modal-overlay"] []
                   [
                     html.div
                       [html.className "toolbox-view"] []
                       [ Toolbox.view 
                           (Html.map (fun m -> ToolboxMsg m) html) toolbox ;
                         html.div
                           [html.className "toolbox-controls"] []
                           [
                             html.button
                               [html.className "toolbox-controls-button"] 
                               [
                                 Html.onMouseClick 
                                   html
                                   (fun evt -> 
                                     DismissToolbox toolbox.original)
                               ]
                               [html.text "Cancel"] ;
                             html.button
                               [html.className "toolbox-controls-button"] 
                               [
                                 Html.onMouseClick
                                   html
                                   (fun evt ->
                                     DismissToolbox toolbox.included)
                               ]
                               [html.text "Update"]
                           ]
                       ]
                   ]
               )
          |> Util.maybeWithDefault
               (html.div [html.className "toolbox-modal-hidden"] [] [])
         ] ;
         dragViz ;
         Controls.view controlHtml state.ui ;
       ]
    )

let combineWithState subkey state =
  let combineWithState_ m state =
    Map.fold 
      (fun state key v ->
         match (key,v) with
         | ("backgroundUrl",SerializeData.String bgurl) -> 
            { state with backgroundUrl = bgurl }
         | ("opacity",SerializeData.Float n) ->
            { state with opacity = n }
         | ("root",root) ->
            { state with root = PanelSerialize.load root ; dirtyPanels = true }
         | _ -> state
      )
      state m
  in
  match subkey with
  | SerializeData.Dict d -> combineWithState_ d state
  | _ -> state

let rec renderPanelToMeasure 
      (getGadgets : Panel -> Gadget<Panel, RenderMsg> list)
      (render : (string * string) list -> RenderMsg list -> Panel -> RenderMsg)
      (ourStyles : (string * string) list)
      (panel : Panel) =
  let renderedChildren =
    List.mapi 
      (fun i p ->
        let layoutStyles =
          List.concat
            (List.map (fun (l : Gadget<Panel, RenderMsg>) -> l.childStyles i p) (getGadgets panel))
        in
        renderPanelToMeasure 
          getGadgets
          render
          layoutStyles
          p
      )
      panel.children
  in
  let localStyles = 
    List.concat 
      (List.map (fun (l : Gadget<Panel, RenderMsg>) -> l.parentStyles panel) (getGadgets panel)) 
  in
  render
    (List.concat [ourStyles; localStyles])
    renderedChildren
    panel

let pushUpdate state =
  VDom.postIFrameMessage 
    "canvas-frame"
    (renderPanelToMeasure
       (fun p -> p.layout)
       (fun sty children panel -> MeasureRender.render sty children panel)
       []
       state.root
    ) ;
  { state with dirtyPanels = false }
             
let clearTimeout state =
  let _ = Timer.clearTimeout state.inactivityTimerId in
  { state with inactivityTimerId = -1 }

let resetTimeout post state = 
  let _ = Timer.clearTimeout state.inactivityTimerId in
  let newTimerId = Timer.setTimeout (fun _ -> post Inactivity) 5000. in
  { state with inactivityTimerId = newTimerId }

let updateWithTimer post msg state =
  begin
    let st = update msg state in
    match msg with
    | Inactivity ->
       begin
         st
         |> save 
         |> Serialize.subkeyToJson 
         |> (fun j -> Serialize.stringify j)
         |> st.localStorage.set "autosave" ;
         st
         |> clearTimeout 
       end
    | LoadAutosave ->
       st.localStorage.get "autosave" 
       ||> Util.expose "autosave"
       ||> (fun s -> 
        Serialize.parse 
          (fun e -> SerializeData.jsnull () |> Serialize.subkeyToJson) s)
       ||> (fun j -> Serialize.jsonToSubkey j)
       ||> (fun sk -> post (AutosaveLoaded sk))
       st 
       |> clearTimeout
    | AutosaveLoaded j ->
       combineWithState j state |> clearTimeout
    | _ -> resetTimeout post st
  end
    
let updateAndEmit post msg state =
  begin
    let st = updateWithTimer post msg state in
    match msg with
    | Measures m -> { st with measure = m }
    | Inactivity -> st
    | LoadAutosave -> st
    | Rerender -> pushUpdate st
    | _ ->
       begin
         Util.log "visualUpdate" (st.dirtyPanels, msg) ;
         if st.dirtyPanels then
           pushUpdate st
         else
           st
       end
  end

let main (vdom : Msg VDom.VDom) arg =
  let post : Msg -> unit = vdom.post in
  { VDom.init = 
      fun arg ->
      begin
        VDom.addWindowMessageHandler
          "measure"
          (fun msg -> vdom.post (Measures (Measure.toMeasure msg))) ;
        VDom.addWindowMessageHandler
          "init"
          (fun msg -> vdom.post Rerender) ;
        Timer.setTimeout (fun _ -> post LoadAutosave) 0. ;
        init arg
      end
    VDom.update = (fun msg state -> updateAndEmit post msg state) ;
    VDom.view = (fun state -> view (Html.html vdom) state) ;
  }
