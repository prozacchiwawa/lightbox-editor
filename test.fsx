#r "node_modules/fable-core/Fable.Core.dll"
   
open Fable.Core
open Fable.Import.Browser

#load "util.fs"
#load "vdom.fs"
#load "html.fs"
#load "point.fs"
#load "panel.fs"
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
  { 
    palette = Map<string, Color> [] ;
    selected = "root" ;
    root = { 
        Panel.position = Panel.Absolute ;
        Panel.background = "" ;
        Panel.lr = Panel.MidCover (0.,1000.) ;
        Panel.tb = Panel.MidCover (0.,1000.) ;
        Panel.id = "root" ;
        Panel.children = [] ;
      } ;
    dragger = None ;
    dragmode = Select ;
    ui = { full = false }
  }

let update action state =
  let selectPanel coords =
    match Panel.fromCoord coords state.root with
    | [] -> state
    | hd :: tl -> 
       if hd.id = state.selected then
         { state with selected = "" }
       else 
         { state with selected = hd.id }
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
  match (action,state.dragger) with
  | (NoOp,_) -> state
  | (MouseDown (x,y),None) ->
     { state with dragger = Some { start = Point.ctor x y; dend = Point.ctor x y; action = Click } }
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
  | (ControlMsg msg,_) -> 
     { state with dragger = None; ui = Controls.update msg state.ui }
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
        {
          name = "className"; value = panelClass};
          html.style [
            ("position", Panel.positionString panel);
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
           [ {name = "className"; value = "dragger" };
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
  html.div
    [{name = "className"; value = "root"}]
    [ 
      Html.onMouseDown html (fun evt -> MouseDown (evt.pageX, evt.pageY)) ;
      Html.onMouseUp html (fun evt -> MouseUp (evt.pageX, evt.pageY)) ;
      Html.onMouseMove html (fun evt -> MouseMove (evt.pageX, evt.pageY))
    ]
    (List.concat 
       [
         Controls.view controlHtml state.ui ;
         visualizeDrag ;
         [ viewPanel state.root ] ;
       ]
    )
    
let main vdom arg =
  { VDom.init = init;
    VDom.update = update;
    VDom.view = (view (Html.html vdom))
  }
