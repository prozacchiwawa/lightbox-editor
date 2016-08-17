#r "node_modules/fable-core/Fable.Core.dll"
   
open Fable.Core
open Fable.Import.Browser

#load "util.fs"
#load "vdom.fs"
#load "html.fs"
#load "point.fs"
#load "panel.fs"
#load "input.fs"
#load "controls.fs"

type Point = Point.Point
type Panel = Panel.Panel
type UI = Controls.UI

type Color = { r : int ; b : int ; g : int ; a : int }
               
type DragMode =
  | Select
               
type DragAction =
  | Click
  | Drag

type Dragger = { start : Point; dend : Point; action : DragAction } 
                 
type State = 
  { 
    palette : Map<string, Color> ;
    root : Panel ;
    selected : string ;
    backgroundUrl : string ;
    dragmode : DragMode ;
    dragger : Dragger option ;
    ui : UI ;
  }
    
type Msg = 
  | NoOp 
  | AddChild of string
  | RemovePanel of string
  | MouseDown of float * float
  | MouseUp of float * float
  | MouseMove of float * float
  | ControlMsg of Controls.Msg
                             
let init arg =
  let root = 
    { 
      Panel.position = Panel.Absolute ;
      Panel.background = "" ;
      Panel.lr = Panel.MidCover (0.,1000.) ;
      Panel.tb = Panel.MidCover (0.,1000.) ;
      Panel.id = "root" ;
      Panel.children = [] ;
    }
  in
  { 
    palette = Map<string, Color> [] ;
    selected = "root" ;
    backgroundUrl = "" ;
    root = root ;
    dragger = None ;
    dragmode = Select ;
    ui = Controls.init root
  }

let update action state =
  let selectPanel coords =
    match Panel.fromCoord coords state.root with
    | [] -> state
    | hd :: tl -> 
       if hd.id = state.selected then
         { state with selected = "" ; ui = Controls.select state.root state.ui }
       else 
         { state with selected = hd.id ; ui = Controls.select hd state.ui }
  in
  let createNewPanel parentCoords dragger =
    let draggerUL = 
      Point.ctor
        (Util.min 0. [dragger.start.x;dragger.dend.x])
        (Util.min 0. [dragger.start.y;dragger.dend.y])
    in
    let draggerBR = 
      Point.ctor
        (Util.max 0. [dragger.start.x;dragger.dend.x])
        (Util.max 0. [dragger.start.y;dragger.dend.y])
    in
    let finalUL = Point.subtract draggerUL parentCoords in
    let finalBR = Point.subtract draggerBR parentCoords in
    { Panel.lr = Panel.LowGrav (finalUL.x, finalBR.x - finalUL.x) ;
      Panel.tb = Panel.LowGrav (finalUL.y, finalBR.y - finalUL.y) ;
      Panel.position = Panel.Absolute ;
      Panel.background = "" ;
      Panel.id = Util.genId () ;
      Panel.children = [] 
    }
  in
  let rec addChildWithId id child (parent : Panel) =
    if parent.id = id then
      { parent with Panel.children = child :: parent.children }
    else
      { parent with Panel.children = List.map (addChildWithId id child) parent.children }
  in
  let createChild dragger =
    match Panel.fromCoord dragger.start state.root with
    | [] -> state
    | hd :: tl ->
       match Panel.offset hd.id state.root with
       | [] -> state
       | parentCoords :: _ ->
          { state with root = addChildWithId hd.id (createNewPanel parentCoords dragger) state.root }
  let performDragOp dragger = 
    match (Util.log "PerformDragOp" dragger.action) with
    | Click -> selectPanel dragger.start
    | Drag -> createChild dragger
  in
  match Util.expose "action" (action,state.dragger) with
  | (NoOp,_) -> state
  | (MouseDown (x,y),None) ->
     if x >= 0. && x < 1000. && y >= 0. && y < 1000. then
       { state with dragger = Some { start = Point.ctor x y; dend = Point.ctor x y; action = Click } }
     else 
       state
  | (MouseUp (x,y),Some dragger) ->
     { performDragOp dragger with dragger = None }
  | (MouseMove (x,y),Some dragger) ->
     let draggerUL = 
       Point.ctor
         (Util.min 0. [dragger.start.x;dragger.dend.x])
         (Util.min 0. [dragger.start.y;dragger.dend.y])
     in
     let draggerBR = 
       Point.ctor
         (Util.max 0. [dragger.start.x;dragger.dend.x])
         (Util.max 0. [dragger.start.y;dragger.dend.y])
     in
     let manhDistance = 
       (draggerBR.x - draggerUL.x) + (draggerBR.y - draggerUL.y) 
     in
     if manhDistance > 4. then
       { state with dragger = Some { dragger with dend = Point.ctor x y; action = Drag } }
     else 
       { state with dragger = Some { dragger with dend = Point.ctor x y } }
  | (ControlMsg (Controls.ChangeBackground bg),_) ->
     { state with backgroundUrl = Util.log "Background" bg }
  | (ControlMsg msg,_) -> 
     let s1 = { state with dragger = None; ui = Controls.update msg state.ui } in
     if s1.ui.dirtyPanel then
       let (panel,ui) = Controls.takeUpdate s1.ui in
       { s1 with ui = ui ; root = Panel.replace panel s1.root }
     else
       s1
  | _ -> state
         
let cssPixelPos v =
  String.concat "" [Util.toString v; "px"]
                
let view (html : Msg Html.Html) state =
  let rec viewPanel (panel : Panel) =
    let panelClass = 
      if state.selected = panel.id then 
        String.concat " " ["panel";"panel-selected"]
      else 
        "panel" 
    in
    let draggerUL = Panel.upperLeft panel in
    let draggerBR = Panel.lowerRight panel in
    html.div
      [
        html.className panelClass ;
        html.style 
          [
            ("position", Panel.positionString panel.position);
            ("left", cssPixelPos draggerUL.x);
            ("top", cssPixelPos draggerUL.y);
            ("width", cssPixelPos (draggerBR.x - draggerUL.x));
            ("height", cssPixelPos (draggerBR.y - draggerUL.y))
          ]
      ]
      []
      (List.concat 
         [
           [html.text (String.concat " " ["PANEL:";panel.id])];
           List.map viewPanel panel.children
         ]
      )
  in
  let visualizeDrag =
    match (state.dragger,state.dragmode) with
    | (None,_) -> []
    | (Some dragger,Select) ->
       let draggerUL = 
         Point.ctor
           (Util.min 0. [dragger.start.x;dragger.dend.x])
           (Util.min 0. [dragger.start.y;dragger.dend.y])
       in
       let draggerBR = 
         Point.ctor
           (Util.max 0. [dragger.start.x;dragger.dend.x])
           (Util.max 0. [dragger.start.y;dragger.dend.y])
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
            ("background-repeat", "none") ;
            ("background-position", "center top") ;
            ("background-size", "contain") ;
          ]
      ]
    else
      []
  in
  html.div
    [html.className "app-container"]
    []
    (List.concat 
       [
         [ 
           html.div
             (List.concat [ [html.className "root"]; backgroundSpecification ])
             [ 
               Html.onMouseDown html (fun evt -> MouseDown (evt.pageX, evt.pageY)) ;
               Html.onMouseUp html (fun evt -> MouseUp (evt.pageX, evt.pageY)) ;
               Html.onMouseMove html (fun evt -> MouseMove (evt.pageX, evt.pageY))
             ]
             [ viewPanel state.root ] ;
         ] ;
         Controls.view controlHtml state.ui ;
         visualizeDrag
       ]
    )

let main vdom arg =
  { VDom.init = init;
    VDom.update = update;
    VDom.view = (view (Html.html vdom))
  }
