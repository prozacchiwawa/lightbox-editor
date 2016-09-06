#r "node_modules/fable-core/Fable.Core.dll"
   
open Fable.Core
open Fable.Import.Browser

#load "util.fs"
#load "point.fs"
#load "grid.fs"
#load "vdom.fs"
#load "html.fs"
#load "serializedata.fs"
#load "css.fs"
#load "gridrenderer.fs"
#load "layoutmgr.fs"
#load "measure.fs"
#load "panel.fs"
#load "layoutmgrimpl.fs"
#load "measurerender.fs"
#load "wireframe.fs"
#load "input.fs"
#load "controlinterface.fs"
#load "panelcontrols.fs"
#load "controls.fs"
#load "q.fs"
#load "storage.fs"
#load "localstorage.fs"
#load "timer.fs"
#load "serialize.fs"
#load "layoutmgrload.fs"
#load "panelserialize.fs"

open Q

type Point = Point.Point
type Panel = Panel.Panel
type UI = Controls.UI
type Grid = Grid.Grid
type FreeLayoutMgr = LayoutMgrImpl.FreeLayoutMgr
type MeasureMsg = Measure.MeasureMsg

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
    dirtyPanels : bool ;
    selected : Panel ;
    backgroundUrl : string ;
    dragmode : DragMode ;
    dragger : Dragger option ;
    grid : Grid ;
    ui : UI ;
    measure : MeasureMsg ;
    inactivityTimerId : int ;
    localStorage : Storage.Storage ;
  }
    
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

let init arg =
  let root = 
    { 
      Panel.position = Panel.Absolute ;
      Panel.background = "" ;
      Panel.lr = Panel.MidCover (0.,1000.) ;
      Panel.tb = Panel.MidCover (0.,1000.) ;
      Panel.id = "root" ;
      Panel.children = [] ;
      Panel.layout = new FreeLayoutMgr() ;
    }
  in
  let grid = Grid.create false (Point.ctor 0. 0.) (Point.ctor 16. 16.) in
  { 
    palette = Map<string, Color> [] ;
    selected = root ;
    backgroundUrl = "" ;
    root = root ;
    dirtyPanels = false ; 
    dragger = None ;
    dragmode = Select ;
    grid = grid ;
    ui = Controls.init grid root ;
    measure = Measure.emptyMeasure ;
    inactivityTimerId = -1 ;
    localStorage = new LocalStorage.LocalStorage() :> Storage.Storage
  }

let save state =
  [ ("backgroundUrl", SerializeData.string state.backgroundUrl) ;
    ("root", PanelSerialize.save state.root)
  ]
  |> SerializeData.map

let update action state =
  let selectPanel (panel : Panel) state =
    let parent = 
      state.root
      |> (Panel.parent panel.id state.root)
      |> Util.headWithDefault state.root
    in
    { state with 
      selected = panel ; 
      ui = 
        Controls.select 
          panel 
          (Util.expose "Controls.select parent" parent) state.ui 
    }
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
      Panel.children = [] ;
      Panel.layout = new FreeLayoutMgr()
    }
  in
  let rec addChildWithId id child (parent : Panel) =
    if parent.id = id then
      { parent with 
        Panel.children = child :: parent.children 
      }
    else
      { parent with 
        Panel.children = List.map (addChildWithId id child) parent.children 
      }
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
               addChildWithId 
                 hd.id 
                 (createNewPanel offset dragger) 
                 state.root ;
             dirtyPanels = true 
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
     let pt = Grid.snap (Point.ctor x y) state.grid in
     if pt.x >= 0. && pt.x < 1000. && pt.y >= 0. && pt.y < 1000. then
       { state with 
         dragger = Some { start = pt; dend = pt; action = Click } 
       }
     else 
       state
  | (MouseUp (x,y),Some dragger) ->
     { performDragOp dragger with dragger = None }
  | (MouseMove (x,y),Some dragger) ->
     let pt = Grid.snap (Point.ctor x y) state.grid in
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
       { state with 
         dragger = 
           Some { dragger with dend = Point.ctor pt.x pt.y; action = Drag } 
       }
     else 
       { state with 
         dragger = 
           Some { dragger with dend = Point.ctor pt.x pt.y } 
       }
  | (ControlMsg (Controls.ChangeBackground bg),_) ->
     { state with backgroundUrl = Util.log "Background" bg }
  | (ControlMsg (Controls.SelectPanel pid),_) ->
     state.root
     |> Panel.fromId pid
     |> Util.headWithDefault state.root
     |> (Util.flip selectPanel) state
  | (ControlMsg (Controls.DeletePanel pid),_) ->
     let reselect = 
       state.root
       |> Panel.fromId pid
       |> Util.headWithDefault state.root
       |> (Util.flip selectPanel) state
     in
     { reselect with root = Panel.remove pid state.root ; dirtyPanels = true }
  | (ControlMsg msg,_) -> 
     let s0 = 
       { state with 
         dragger = None; ui = Controls.update msg state.ui 
       } 
     in
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
  | _ -> state
         
let cssPixelPos v =
  String.concat "" [Util.toString v; "px"]

let view (html : Msg Html.Html) state =
  let visualizeDrag =
    match (state.dragger,state.dragmode) with
    | (None,_) -> 
       [ html.div [{name = "style"; value = "display: none"}] [] [] ]
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
                 [html.className "dragger-container"]
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
                      visualizeDrag ;
                      GridRenderer.view html state.grid
                    ]
                 )
             ] ;
         ] ;
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
         | ("root",root) ->
            { state with root = PanelSerialize.load root ; dirtyPanels = true }
         | _ -> state
      )
      state m
  in
  match subkey with
  | SerializeData.Dict d -> combineWithState_ d state
  | _ -> state

let pushUpdate state =
  VDom.postIFrameMessage 
    "canvas-frame"
    (state.root.layout.view
       []
       (fun p -> p.layout)
       (fun sty children panel -> MeasureRender.render sty children panel)
       state.root
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
    VDom.view = (view (Html.html vdom)) ;
  }
