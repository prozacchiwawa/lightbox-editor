#r "node_modules/fable-core/Fable.Core.dll"
   
open Fable.Core
open Fable.Import.Browser

#load "util.fs"
#load "vdom.fs"
#load "html.fs"

type Color = { r : int ; b : int ; g : int ; a : int }
               
type AxisPosition =
  |   LowGrav of float * float
  |   HighGrav of float * float
  |   MidCover of float * float
                            
type Position =
  |   Absolute
  |   Relative

type Point = { x : float; y : float }

type DragMode =
  | Select
               
type DragAction =
  | Click
  | Drag

type Dragger = { start : Point; dend : Point; action : DragAction } 
                 
type Panel =
  {
    lr : AxisPosition ;
    tb : AxisPosition ;
    position : Position ;
    background : string ;
    id : string ;
    children : Panel list ;
  }
    
type State = 
  { 
    palette : Map<string, Color> ;
    root : Panel ;
    selected : string ;
    dragmode : DragMode ;
    dragger : Dragger option ;
  }
    
type Msg = 
  |   NoOp 
  |   AddChild of string
  |   RemovePanel of string
  |   MouseDown of float * float
  |   MouseUp of float * float
  |   MouseMove of float * float
                             
let init arg =
  { 
    palette = Map<string, Color> [] ;
    selected = "root" ;
    root = { 
        position = Absolute ;
        background = "" ;
        lr = MidCover (0.,1000.) ;
        tb = MidCover (0.,1000.) ;
        id = "root" ;
        children = [] ;
      } ;
    dragger = None ;
    dragmode = Select
  }
    
let between a b c =
  (Util.abs (c - a)) < (Util.abs (b - a))

let coordsInside a b c =
  (between a.x b.x c.x) && (between a.y b.y c.y)

let upperLeftPanel p =
  let l = 
    match p.lr with
    | MidCover (l,r) -> l
    | LowGrav (l,w) -> l
    | HighGrav (w,r) -> (r - w)
  in
  let t =
    match p.tb with
    | MidCover (t,b) -> t
    | LowGrav (t,h) -> t
    | HighGrav (h,b) -> (b - h)
  in 
  { x = l; y = t }

let lowerRightPanel p =
  let r =
    match p.lr with
    | MidCover (l,r) -> r
    | LowGrav (l,w) -> (l + w)
    | HighGrav (w,r) -> r
  in
  let b =
    match p.tb with
    | MidCover (t,b) -> b
    | LowGrav (t,h) -> (t + h)
    | HighGrav (h,b) -> b
  in
  { x = r; y = b }
       
let coordInPanel coords panel =
  let ul = upperLeftPanel panel in
  let lr = lowerRightPanel panel in
  coordsInside ul lr coords

let rec panelsFromCoord coords panel =
  let matchingChildPanels =
    List.concat (List.map (panelsFromCoord coords) panel.children)
  in
  let matchingThisPanel =
    if coordInPanel coords panel then
      [ panel ]
    else 
      []
  in
  List.concat [matchingChildPanels; Util.log "Matched" matchingThisPanel]

let update action state =
  let selectPanel coords =
    match panelsFromCoord coords state.root with
    | [] -> state
    | hd :: tl -> 
       if hd.id = state.selected then
         { state with selected = "" }
       else 
         { state with selected = hd.id }
  in
  let performDragOp dragger = 
    match (Util.log "PerformDragOp" dragger.action) with
    | Click -> selectPanel dragger.start
    | Drag -> state
  in
  match (action,state.dragger) with
  | (NoOp,_) -> state
  | (MouseDown (x,y),None) ->
     { state with dragger = Some { start = { x = x ; y = y }; dend = { x = x; y = y } ; action = Click } }
  | (MouseUp (x,y),Some dragger) ->
     { performDragOp dragger with dragger = None }
  | (MouseMove (x,y),Some dragger) ->
     let draggerUL = 
       { x = Util.min 0. [dragger.start.x;dragger.dend.x]; 
         y = Util.min 0. [dragger.start.y;dragger.dend.y] 
       } 
     in
     let draggerBR = 
       { x = Util.max 0. [dragger.start.x;dragger.dend.x];
           y = Util.max 0. [dragger.start.y;dragger.dend.y] 
       } 
     in
     let manhDistance = 
       (draggerBR.x - draggerUL.x) + (draggerBR.y - draggerUL.y) 
     in
     if manhDistance > 4. then
       { state with dragger = Some { dragger with dend = { x = x; y = y }; action = Drag } }
     else 
       { state with dragger = Some { dragger with dend = { x = x; y = y } } }
  | _ -> state
         
let cssPixelPos v =
  String.concat "" [Util.toString v; "px"]
                
let view (html : Msg Html.Html) state =
  let rec viewPanel panel =
    let panelClass = 
      if state.selected = panel.id then 
        String.concat " " ["panel";"panel-selected"]
      else 
        "panel" 
    in
    html.div
      [{name = "className"; value = panelClass}]
      []
      (List.concat 
         [
           [html.text (String.concat " " ["PANEL:";panel.id])];
           List.map viewPanel panel.children
         ]
      )
  in
  let visualizeDrag state =
    match (state.dragger,state.dragmode) with
    | (None,_) -> []
    | (Some dragger,Select) ->
       let draggerUL = 
         { x = Util.min 0. [dragger.start.x;dragger.dend.x]; 
           y = Util.min 0. [dragger.start.y;dragger.dend.y] 
         } 
       in
       let draggerBR = 
         { x = Util.max 0. [dragger.start.x;dragger.dend.x];
           y = Util.max 0. [dragger.start.y;dragger.dend.y] 
         } 
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
  html.div
    [{name = "className"; value = "root"}]
    [ 
      html.onMouseDown (fun evt -> MouseDown (evt.pageX, evt.pageY)) ;
      html.onMouseUp (fun evt -> MouseUp (evt.pageX, evt.pageY)) ;
      html.onMouseMove (fun evt -> MouseMove (evt.pageX, evt.pageY))
    ]
    (List.concat 
       [
         [ viewPanel state.root ] ;
         visualizeDrag state
       ]
    )
    
let main vdom arg =
  { VDom.init = init;
    VDom.update = update;
    VDom.view = (view (Html.html vdom))
  }
