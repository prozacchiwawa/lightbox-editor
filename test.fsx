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
               
type Dragger = { start : Point; dend : Point } 
                 
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
        lr = MidCover (0.,0.) ;
        tb = MidCover (0.,0.) ;
        id = "root" ;
        children = [] ;
      } ;
    dragger = None       
  }
    
let update action state =
  match action with
  |   NoOp -> state
  |   MouseDown (x,y) ->
       { state with dragger = Some { start = { x = x ; y = y }; dend = { x = x; y = y } } }
  |   MouseUp (x,y) ->
       { state with dragger = None }
  |   MouseMove (x,y) ->
       match state.dragger with
       | None -> state
       | Some dragger ->
          { state with dragger = Some { dragger with dend = { x = x; y = y } } }
         
let cssPixelPos v =
  String.concat "" [Util.toString v; "px"]
                
let view (html : Msg Html.Html) state =
  let rec viewPanel panel =
    html.div
      [{name = "className"; value = "panel"}]
      []
      (List.concat 
         [
           [html.text (String.concat " " ["PANEL:";panel.id])];
           List.map viewPanel panel.children
         ]
      )
  in
  let visualizeDrag dragger =
    match dragger with
    | None -> []
    | Some dragger ->
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
         visualizeDrag state.dragger
       ]
    )
    
let main vdom arg =
  { VDom.init = init;
    VDom.update = update;
    VDom.view = (view (Html.html vdom))
  }
