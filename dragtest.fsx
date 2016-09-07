#r "node_modules/fable-core/Fable.Core.dll"

open Fable.Core
open Fable.Import.Browser

#load "util.fs"
#load "point.fs"
#load "vdom.fs"
#load "html.fs"
#load "dragcontroller.fs"

open Point
open VDom
open DragController

type Msg =
  | NoOp
  | MouseDown of Point
  | MouseMove of Point
  | MouseUp of Point
  | DragStart of (Point * int)
  | DragMove of (Point * Point * int * int)
  | DragEnd of (Point * Point * int * int)
and Draggable =
  {
    id : int ;
    at : Point ;
    drawn : Point option ;
    color : string ;
    dragging : bool ;
    hovering : bool 
  }

type State =
  {
    dragger : (int, State, Msg) DragController.State ;
    objects : Draggable list
  }

let init arg =
  {
    dragger = emptyDragController NoOp ;
    objects = 
      [
        { id = 1 ; at = { x = 100.0; y = 100.0; }; drawn = None; color = "red" ; dragging = false ; hovering = false } ;
        { id = 2 ; at = { x = 300.0; y = 100.0; }; drawn = None; color = "blue" ; dragging = false ; hovering = false } ;
        { id = 3 ; at = { x = 200.0; y = 300.0; }; drawn = None; color = "green" ; dragging = false ; hovering = false } ;
        { id = 4 ; at = { x = 100.0; y = 400.0; }; drawn = None; color = "yellow" ; dragging = false ; hovering = false }
      ]
  }

let setDragging id v state =
  { state with 
    objects = 
      List.map 
        (fun o -> if o.id = id then { o with dragging = v } else o) 
        state.objects
  }

let setHovering id v state =
  { state with
    objects =
      List.map
        (fun o -> if o.id = id then { o with hovering = v } else o)
        state.objects
  }

let setDragPosition id st pt state =
  { state with
    objects =
      List.map
        (fun o -> if o.id = id then { o with drawn = Some { x = o.at.x + (pt.x - st.x) ; y = o.at.y + (pt.y - st.y) } } else o)
        state.objects
  }

let commitDrag id st pt state =
  { state with
    objects =
      List.map
        (fun o -> if o.id = id then { o with drawn = None ; at = match o.drawn with Some pt -> pt | None -> o.at } else o)
        state.objects
  }

let rec update msg state =
  let takeMessages messages state =
    List.fold
      (fun state msg -> update msg state)
      state
      messages
  in
  let dragMsg =
    match msg with
    | MouseDown pt -> Some (DragController.MouseDown pt)
    | MouseMove pt -> Some (DragController.MouseMove pt)
    | MouseUp pt -> Some (DragController.MouseUp)
    | _ -> None
  in
  match (msg, dragMsg) with
  | (_, Some dm) ->
     let report = DragController.update dm state state.dragger in
     takeMessages 
       (List.rev report.dispatched) 
       { state with dragger = report.dragger }
  | (DragStart (pt,id), _) ->
     state |> setDragging id true
  | (DragMove (st,pt,id,hov), _) ->
     state |> setDragPosition id st pt
  | (DragEnd (st,pt,id,hov), _) ->
     state |> commitDrag id st pt 
  | _ -> state

let viewDragObject (html : Msg Html.Html) draggable =
  html.div
    [
      html.style 
        [
          ("position", "absolute") ;
          ("left", String.concat "" [Util.toString draggable.at.x; "px"]) ;
          ("top", String.concat "" [Util.toString draggable.at.y; "px"]) ;
          ("width", "100px") ;
          ("height", "100px") ;
          ("color", "white") ;
          ("border", "1px solid black") ;
          ("background-color", draggable.color)
        ]
    ] []
    [html.text "Drag me"]

let view (html : Msg Html.Html) state =
  html.div 
    [] [] [
      html.div
        [] 
        [
          Html.onMouseDown 
            html 
            (fun evt -> MouseDown { x = evt.pageX ; y = evt.pageY }) ;
          Html.onMouseMove 
            html 
            (fun evt -> MouseMove { x = evt.pageX ; y = evt.pageY }) ;
          Html.onMouseUp
            html
            (fun evt -> MouseUp { x = evt.pageX ; y = evt.pageY })
        ]
        [] ;
      html.div
        []
        []
        (List.map (viewDragObject html) state.objects)
    ]

let main (vdom : Msg VDom.VDom) arg =
  let post : Msg -> unit = vdom.post in
  { VDom.init = init ;
    VDom.update = (fun msg state -> update msg state) ;
    VDom.view = (view (Html.html vdom)) ;
  }
