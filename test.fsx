#r "node_modules/fable-core/Fable.Core.dll"
   
open Fable.Core
open Fable.Import.Browser

#load "util.fs"
#load "point.fs"
#load "grid.fs"
#load "vdom.fs"
#load "html.fs"
#load "css.fs"
#load "panel.fs"
#load "input.fs"
#load "controls.fs"

type Point = Point.Point
type Panel = Panel.Panel
type UI = Controls.UI
type Grid = Grid.Grid

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
    selected : Panel ;
    backgroundUrl : string ;
    dragmode : DragMode ;
    dragger : Dragger option ;
    grid : Grid ;
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
  | PanelMsg of Panel.Msg

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
  let grid = Grid.create false (Point.ctor 0. 0.) (Point.ctor 1. 1.) in
  { 
    palette = Map<string, Color> [] ;
    selected = root ;
    backgroundUrl = "" ;
    root = root ;
    dragger = None ;
    dragmode = Select ;
    grid = grid ;
    ui = Controls.init grid root
  }

let update action state =
  let selectPanel (panel : Panel) state =
    let parent = 
      state.root
      |> (Panel.parent panel.id state.root)
      |> Util.headWithDefault state.root
    in
    { state with selected = panel ; ui = Controls.select panel (Util.expose "Controls.select parent" parent) state.ui }
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
    state.root
    |> Panel.fromCoord dragger.start
    |> Util.andMap
         (fun hd -> 
           state.root 
           |> Panel.offset hd.id 
           |> List.map (Util.tuple2 hd)
         )
    |> List.map 
         (fun (hd, offset) ->
           { state with 
             root = 
               addChildWithId hd.id (createNewPanel offset dragger) state.root
           }
         )
    |> Util.headWithDefault state 
  in
  let performDragOp dragger = 
    match (Util.log "PerformDragOp" dragger.action) with
    | Click -> 
       state.root
       |> Panel.fromCoord dragger.start
       |> List.map 
            (fun panel -> 
              if state.selected.id = panel.id then
                state.root 
              else 
                panel
            )
       |> Util.headWithDefault state.root
       |> (Util.flip selectPanel) state
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
  | (ControlMsg (Controls.SelectPanel pid),_) ->
     state.root
     |> Panel.fromId pid
     |> Util.headWithDefault state.root
     |> (Util.flip selectPanel) state
  | (ControlMsg (Controls.SetGrid grid),_) ->
     { state with grid = grid }
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
             [html.className "root-container"] []
             [
               html.div
                 (List.concat [ [html.className "root"]; backgroundSpecification ])
                 [ 
                   Html.onMouseDown html (fun evt -> MouseDown (evt.pageX, evt.pageY)) ;
                   Html.onMouseUp html (fun evt -> MouseUp (evt.pageX, evt.pageY)) ;
                   Html.onMouseMove html (fun evt -> MouseMove (evt.pageX, evt.pageY))
                 ]
                 [ Panel.view (Html.map (fun msg -> PanelMsg msg) html) state.selected.id state.root ] ;
             ] ;
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
