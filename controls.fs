module Controls

open Util
open Html
open Panel
open Input

type Panel = Panel.Panel

type Msg =
  | NoOp
  | ToggleControls
  | BackgroundInput of string
  | ChangeBackground of string
  | ChangePanel of Panel
  | SelectPanel of string
  | EditorMsg of Input.Msg

type UI =
  {
    full : bool ;
    backgroundUrl : string ;
    focused : Panel ;
    editors : Input.EditorSet ;
    dirtyPanel : bool ;
  }

let makeEditors panel =
  let ul = Panel.upperLeft panel in
  let lr = Panel.lowerRight panel in
  let createEditor name value = 
    new InputEditor(
          value, 
          (fun value -> (Util.parseFloat value) <> None), 
          ("numeric-input-good", "numeric-input-bad")
        )
  in
  List.fold 
    (fun editors (name,value) -> Input.create name (createEditor name value) editors)
    (Input.init ())
    [
      ("Left", Util.toString ul.x);
      ("Top", Util.toString ul.y);
      ("Right", Util.toString lr.x);
      ("Bottom", Util.toString lr.y);
    ]
    
let init panel = 
  { full = false ; 
    backgroundUrl = "" ; 
    focused = panel ; 
    editors = makeEditors panel ;
    dirtyPanel = false ;
  }

let select panel state =
  { state with focused = panel ; editors = makeEditors panel }

let updatePanelWithValue name value panel =
  match (name,value) with
  | ("Left",Some v) -> Panel.setLeft v panel
  | ("Right",Some v) -> Panel.setRight v panel
  | ("Top",Some v) -> Panel.setTop v panel
  | ("Bottom",Some v) -> Panel.setBottom v panel
    
let updatePanelFromEditor state =
  { state with
    dirtyPanel = true ;
    focused = 
      List.fold 
        (fun panel ((name,editor) : (string * Input.EditorInstance)) ->
          let v = Util.parseFloat (editor.currentValue ()) in
          updatePanelWithValue name v panel)
        state.focused
        (Input.map (fun n v -> (n,v)) state.editors)
  }

let update action state =
  match action with
  | ToggleControls ->
     { state with full = not state.full }
  | BackgroundInput bg ->
     { state with backgroundUrl = bg }
  | EditorMsg msg ->
     { state with editors = Input.update msg state.editors } |> 
       updatePanelFromEditor
  | _ -> state

let rootView (html : Msg Html) state =
  [ html.div [html.className "control-row"] [] [ html.text "Root" ] ;
    html.div [html.className "control-row"] [] [ html.text "Background Image" ] ; 
    html.div 
      [html.className "control-row"] [] 
      [ 
        html.input 
          [html.inputValue state.backgroundUrl] 
          [Html.onInput html (fun evt -> BackgroundInput evt.target.value)] 
          []; 
        html.button 
          [] 
          [Html.onMouseClick html (fun evt -> ChangeBackground state.backgroundUrl)]
          [html.text "Set Background"]
      ]
  ]

let rec panelChildren (html : Msg Html) panel =
  html.div
    [html.className "panel-tree-row"]
    [Html.onMouseClick html (fun evt -> SelectPanel panel.id)]
    (List.concat
       [
         [ html.text (String.concat " " ["Panel";panel.id]) ];
         List.map (panelChildren html) panel.children
       ]
    )

let panelControls (html : Msg Html) state panel =
  let labeledInput name (inp : Input.EditorInstance) =
    [
      html.div
        [html.className "control-row"] []
        [html.text name];
      html.div
        [html.className "control-row"] []
        [inp.view (Html.map (fun msg -> EditorMsg msg) html) name]
    ]
  in
  let inputs =
    Input.map labeledInput state.editors
  in
  html.div
    [] []
    [
      html.text (String.concat " " ["Panel"; state.focused.id]);
      html.div
        [] []
        (List.concat inputs)
    ]


let panelView (html : Msg Html) state =
  [ 
    html.div 
      [] [] 
      [
        panelControls html state state.focused ;
        panelChildren html state.focused
      ]
  ]

let view (html : Msg Html) state =
  let controlClass =
    if state.full then
      "control-open"
    else
      "control-closed"
  in
  let controlView = 
    if state.focused.id = "root" then 
      fun h s -> rootView h s 
    else 
      fun h s -> panelView h s
  in
  [
    html.div
      [ html.className "control-toggle" ]
      [ Html.onMouseClick html (fun evt -> ToggleControls) ]
      [ 
        html.i
          [ html.className "fa fa-bars" ;
            {name = "aria-hidden"; value = "true"} ] [] [] ;
      ] ;
    html.div
      [ html.className controlClass ] []
      [ 
        html.div 
          [html.className "control-width"]
          []
          (controlView html state)
      ]
  ]

let takeUpdate state =
  Util.expose "panelUpdate" (state.focused, { state with dirtyPanel = false })
