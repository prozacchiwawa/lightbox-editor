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
  | DragEnter of (Point * Point * int)
  | DragLeave of (Point * Point * int)
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
  let findByPoint pt state =
    let found = 
      List.filter 
        (fun d -> 
          Point.inside d.at (Point.add d.at { x = 100.0 ; y = 100.0 }) pt)
        state.objects
    in
    match Util.expose "found" found with
    | hd :: _ -> Some hd.id
    | _ -> None
  in
  {
    dragger = 
      { emptyDragController NoOp with
        getSubject = findByPoint ;
        getTarget = findByPoint ;
        isSameObject = fun state x y -> x = y ;
        authorPickupMsg = fun pt state id -> DragStart (pt,id) ;
        authorMoveMsg = fun st pt state tgt id -> DragMove (st,pt,Util.maybeWithDefault -1 tgt,id) ;
        authorDropMsg = fun st pt state tgt id -> DragEnd (st,pt,Util.maybeWithDefault -1 tgt,id) ;
        authorEnterMsg = fun st pt state tgt -> DragEnter (st,pt,tgt) ;
        authorLeaveMsg = fun st pt state tgt -> DragLeave (st,pt,tgt)
      } ;
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
        (fun o -> if o.id = id then { o with dragging = false ; drawn = None ; at = match o.drawn with Some pt -> pt | None -> o.at } else o)
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
  match Util.expose "msg" (msg, dragMsg) with
  | (_, Some dm) ->
     let report = DragController.update dm state state.dragger in
     takeMessages 
       (List.rev report.dispatched) 
       { state with dragger = report.dragger }
  | (DragStart (pt,id), _) ->
     state |> setDragging id true
  | (DragMove (st,pt,hov,id), _) ->
     state |> setDragPosition id st pt
  | (DragEnd (st,pt,hov,id), _) ->
     state |> commitDrag id st pt 
  | (DragEnter (st,pt,hov), _) ->
     state |> setHovering hov true
  | (DragLeave (st,pt,hov), _) ->
     state |> setHovering hov false
  | _ -> state

let viewDragObject (html : Msg Html.Html) draggable =
  let pos = Util.maybeWithDefault draggable.at draggable.drawn in
  html.div
    [
      html.style 
        [
          ("position", "absolute") ;
          ("box-sizing", "border-box") ;
          ("left", String.concat "" [Util.toString pos.x; "px"]) ;
          ("top", String.concat "" [Util.toString pos.y; "px"]) ;
          ("width", "100px") ;
          ("height", "100px") ;
          ("color", "white") ;
          ("border", if draggable.hovering then "2px solid white" else "1px solid black") ;
          ("z-index", if draggable.dragging then "2" else "1") ;
          ("background-color", draggable.color)
        ]
    ] []
    [html.text "Drag me"]

let view (html : Msg Html.Html) state =
  html.div 
    [] [] [
      html.div
        [
          html.style 
            [ ("position", "absolute") ;
              ("left", "0") ;
              ("top", "0") ;
              ("width", "100%") ;
              ("height", "100%") ;
              ("z-index", "2")
            ]
        ] 
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
        [
          html.style
            [ ("position", "absolute") ;
              ("left", "0") ;
              ("top", "0") ;
              ("width", "100%") ;
              ("height", "100%") ;
              ("z-index", "1")
            ]
        ]
        []
        (List.map (viewDragObject html) state.objects)
    ]

let main (vdom : Msg VDom.VDom) arg =
  let post : Msg -> unit = vdom.post in
  { VDom.init = init ;
    VDom.update = (fun msg state -> update msg state) ;
    VDom.view = (view (Html.html vdom)) ;
  }
