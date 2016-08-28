module Controls

open Util
open Grid
open Html
open Panel
open Input

type Grid = Grid.Grid
type Panel = Panel.Panel

type Msg =
  | NoOp
  | ToggleControls
  | BackgroundInput of string
  | ChangeBackground of string
  | ChangePanel of Panel
  | SelectPanel of string
  | EditorMsg of Input.Msg
  | RootEdMsg of Input.Msg
  | SetGrid of Grid
  | DeletePanel of string

type UI =
  {
    full : bool ;
    backgroundUrl : string ;
    parent : Panel ;
    focused : Panel ;
    editors : Input.EditorSet ;
    rootEd : Input.EditorSet ;
    dirtyPanel : bool ;
    grid : Grid ;
  }

let foldInto f list init = List.fold f init list

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
  let createSelector name value vlist =
    new InputSelector(
          value,
          vlist
        )
  in
  (Input.init ()) |>
    foldInto
      (fun editors (name,value) -> 
        Input.create name (createEditor name value) editors
      )
      [
        ("Left", Util.toString ul.x);
        ("Top", Util.toString ul.y);
        ("Right", Util.toString lr.x);
        ("Bottom", Util.toString lr.y);
      ] |>
    foldInto
      (fun editors (name,value,vlist) -> 
        let selector = createSelector name value (new Set<string>(vlist)) in
        Input.create name selector editors
      )
      [
        ("Horizontal Pos", 
         Panel.xAxisPositionString panel.lr,
         List.map Panel.xAxisPositionString Panel.gravityList);
        ("Vertical Pos",
         Panel.yAxisPositionString panel.tb,
         List.map Panel.yAxisPositionString Panel.gravityList);
      ]

let makeRootEditors backgroundUrl grid =
  let createEditor name value = 
    new InputEditor(
          value, 
          (fun value -> true),
          ("text-input", "text-input")
        )
  in
  let createNumEditor name value =
    new InputEditor(
          value, 
          (fun value -> (Util.parseFloat value) <> None), 
          ("numeric-input-good", "numeric-input-bad")
        )
  in
  let createCheckbox name value = 
    new Checkbox(if value then "true" else "false")
  in
  (Input.init ()) |>
    foldInto
      (fun editors (name,value) ->
        let ed = createCheckbox name value in
        Input.create name ed editors
      )
      [
        ("Use Grid", grid.enabled)
      ] |>
    foldInto
      (fun editors (name,value) ->
        let ed = createNumEditor name value in
        Input.create name ed editors
      )
      [
        ("Grid X Offset", Util.toString grid.offset.x) ;
        ("Grid Y Offset", Util.toString grid.offset.y) ;
        ("Grid Width", Util.toString grid.interval.x) ;
        ("Grid Height", Util.toString grid.interval.y)
      ]

let init grid panel = 
  { full = false ; 
    backgroundUrl = "" ;
    parent = panel ;
    focused = panel ; 
    editors = makeEditors panel ;
    rootEd = makeRootEditors "" grid ;
    dirtyPanel = false ;
    grid = grid ;
  }

let select panel parent state =
  let _ = Util.log "Controls.select with" (panel.id, parent.id) in
  { state with focused = panel ; parent = parent ; editors = makeEditors panel }

let updatePanelWithValue name current value panel =
  match Util.expose "updatePanelWithValue" (name,current,value) with
  | ("Left",_,Some v) -> Panel.setLeft v panel
  | ("Right",_,Some v) -> Panel.setRight v panel
  | ("Top",_,Some v) -> Panel.setTop v panel
  | ("Bottom",_,Some v) -> Panel.setBottom v panel
  | ("Horizontal Pos",cv,_) -> 
     let ul = Panel.upperLeft panel in
     let lr = Panel.lowerRight panel in
     let measure = Panel.xAxisStringToGravity cv ul.x lr.x in
     Panel.setLRMeasure measure panel
  | ("Vertical Pos",cv,_) ->
     let ul = Panel.upperLeft panel in
     let lr = Panel.lowerRight panel in
     let measure = Panel.yAxisStringToGravity cv ul.y lr.y in
     Panel.setTBMeasure measure panel
  | _ -> panel

let updateRootWithValue name current value state =
  match (name,current,value) with
  | ("Use Grid",current,_) -> { state with grid = { state.grid with enabled = current <> "false" } }
  | ("Grid X Offset",_,Some value) -> { state with grid = { state.grid with offset = { state.grid.offset with x = value } } }
  | ("Grid Y Offset",_,Some value) -> { state with grid = { state.grid with offset = { state.grid.offset with y = value } } }
  | ("Grid Width",_,Some value) -> { state with grid = { state.grid with interval = { state.grid.interval with x = value } } }
  | ("Grid Height",_,Some value) -> { state with grid = { state.grid with interval = { state.grid.interval with y = value } } }
  | _ -> state
    
let updatePanelFromEditor state =
  { state with
    dirtyPanel = true ;
    focused = 
      List.fold 
        (fun panel ((name,editor) : (string * Input.EditorInstance)) ->
          let cv = Util.log "cv" (editor.currentValue ()) in
          let v = Util.log "v" (Util.parseFloat cv) in
          updatePanelWithValue (Util.log "name" name) cv v panel
        )
        state.focused
        (Input.map Util.tuple2 state.editors)
  }

let updateFromRootEditor state =
  List.fold 
    (fun state ((name,editor) : (string * Input.EditorInstance)) ->
      let cv = editor.currentValue () in
      let v = Util.parseFloat cv in
      updateRootWithValue name cv v state
    )
    state
    (Input.map Util.tuple2 state.rootEd)

let update action state =
  match action with
  | ToggleControls ->
     { state with full = not state.full }
  | BackgroundInput inp ->
     { state with backgroundUrl = inp }
  | EditorMsg msg ->
     { state with editors = Input.update msg state.editors } |> 
       updatePanelFromEditor
  | RootEdMsg msg ->
     { state with rootEd = Input.update msg state.rootEd } |>
       updateFromRootEditor
  | _ -> state

let labeledInput html f name (inp : Input.EditorInstance) =
  [
    html.div
      [html.className "control-row"] []
      [html.text name];
    html.div
      [html.className "control-row"] []
      [inp.view (Html.map f html) name]
  ]

let rootView (html : Msg Html) state =
  let inputs =
    Input.map (labeledInput html (fun msg -> RootEdMsg msg)) state.rootEd
  in
  (List.concat
     [
       [
         html.text "Root";
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
       ] ;
       (List.concat inputs)
     ]
  )

let panelDisplayHierRow (html : Msg Html) panel children =
  html.div
    [html.className "panel-tree-row"]
    [Html.onMouseClick 
       html 
       (fun evt -> 
         begin
           VDom.preventDefault evt ;
           VDom.stopPropagation evt ;
           SelectPanel panel.id
         end
       )
    ]
    (List.concat 
       [
         [ html.text (String.concat " " ["Panel";panel.id]) ];
         children
       ]
    )

let rec panelChildren (html : Msg Html) panel =
  panelDisplayHierRow html panel (List.map (panelChildren html) panel.children)

let panelControls (html : Msg Html) state panel =
  let inputs =
    Input.map (labeledInput html (fun msg -> EditorMsg msg)) state.editors
  in
  html.div
    [] []
    [
      html.text (String.concat " " ["Panel"; state.focused.id]);
      html.div [] [] (List.concat inputs)
    ]

let panelView (html : Msg Html) state =
  [ 
    html.div 
      [] [] 
      [
        panelControls html state state.focused ;
        panelDisplayHierRow html state.parent [panelChildren html state.focused]
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
      []
      [ 
        html.i
          [ html.className "fa fa-trash-o" ;
            {name = "aria-hidden"; value = "true"} ] 
          [ Html.onMouseClick html (fun evt -> DeletePanel state.focused.id) ] 
          [] ;
        html.i
          [ html.className "fa fa-bars" ;
            {name = "aria-hidden"; value = "true"} ]
          [ Html.onMouseClick html (fun evt -> ToggleControls) ]
          [] ;
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
