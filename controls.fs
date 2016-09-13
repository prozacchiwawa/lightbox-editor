module Controls

open Util
open Grid
open Html
open Panel
open Input
open PanelControls

type Grid = Grid.Grid
type Panel = Panel.Panel

type Msg =
  | NoOp
  | ToggleControls
  | BackgroundInput of string
  | ChangeBackground of string
  | ChangePanel of Panel
  | SelectPanel of string
  | RootEdMsg of Input.Msg
  | SetGrid of Grid
  | DeletePanel of string
  | ControlFragMsg of string * PanelControls.Msg
  | ToggleSection of string

type UISectionContainer =
  {
    name : string ;
    hidden : bool ;
    fragment : ControlInterface.ControlInterface<Panel,Msg>
  }

type UI =
  {
    full : bool ;
    backgroundUrl : string ;
    root : Panel ;
    focused : Panel ;
    rootEd : Input.EditorSet ;
    dirtyPanel : bool ;
    grid : Grid ;
    sections : Map<string, UISectionContainer>
  }

type ControlItem =
  {
    name : string ;
    control : ControlInterface.ControlInterface<Panel,Msg>
  }

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

let createEditors panel =
  Map
    [
      (
        "Panel",
        { name = "Panel" ;
          hidden = false ;
          fragment = 
            new PanelControls<Msg>
              (panel, 
               false, 
               PanelControls.makeEditors panel, 
               Html.map (fun msg -> ControlFragMsg ("Panel", msg)),
               fun msg ->
                 match msg with
                 | ControlFragMsg ("Panel", msg) -> Some msg
                 | _ -> None
              )
        }
      )
    ]
    
let init grid panel = 
  { full = false ; 
    backgroundUrl = "" ;
    root = panel ;
    focused = panel ; 
    rootEd = makeRootEditors "" grid ;
    dirtyPanel = false ;
    grid = grid ;
    sections = createEditors panel
  }

let select panel root state =
  let _ = Util.log "Controls.select with" (panel.id, root.id) in
  { state with 
    focused = panel ; 
    root = root ; 
    sections = createEditors panel
  }

let updateRootWithValue name current value state =
  match (name,current,value) with
  | ("Use Grid",current,_) -> { state with grid = { state.grid with enabled = current <> "false" } }
  | ("Grid X Offset",_,Some value) -> { state with grid = { state.grid with offset = { state.grid.offset with x = value } } }
  | ("Grid Y Offset",_,Some value) -> { state with grid = { state.grid with offset = { state.grid.offset with y = value } } }
  | ("Grid Width",_,Some value) -> { state with grid = { state.grid with interval = { state.grid.interval with x = value } } }
  | ("Grid Height",_,Some value) -> { state with grid = { state.grid with interval = { state.grid.interval with y = value } } }
  | _ -> state

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
  | RootEdMsg msg ->
     { state with rootEd = Input.update msg state.rootEd } |>
       updateFromRootEditor
  | ToggleSection name ->
     { state with 
       sections =
         match Map.tryFind name state.sections with
         | None -> state.sections
         | Some v -> Map.add name { v with hidden = not v.hidden } state.sections
     }
  | ControlFragMsg (name,msg) ->
     match Map.tryFind name state.sections with
     | None -> state
     | Some v -> 
        let updated = v.fragment.update action in
        if updated.dirty () then
          let (panel, newEditor) = updated.take () in
          { state with
            focused = panel ;
            dirtyPanel = true ;
            sections =
              Map.add name { v with fragment = newEditor } state.sections
          }
        else
          { state with 
            sections = 
              Map.add name { v with fragment = updated } state.sections
          }
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

let rec panelDisplayHierRow (html : Msg Html) state panel =
  let treeRowClass =
    if state.focused.id = panel.id then
      "panel-tree-row-selected"
    else
      "panel-tree-row"
  in
  html.div
    [html.className treeRowClass]
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
         panel.children
         |> List.rev
         |> List.map (panelDisplayHierRow html state) 
       ]
    )

let panelView (html : Msg Html) state =
  let viewComponent html name ui =
    html.div 
      [] []
      (List.concat
         [
           [ 
             html.div 
               [] [
                 Html.onMouseClick 
                   html 
                   (fun evt -> VDom.stopPropagation evt ; ToggleSection name)
               ]
               [
                 (if ui.hidden then
                    html.i
                      [ 
                        html.className "fa fa-caret-square-o-right" ;
                        {name = "aria-hidden"; value = "true"} 
                      ] [] []
                  else
                    html.i
                      [
                        html.className "fa fa-caret-square-o-down" ;
                        {name = "aria-hidden"; value = "true"}
                      ] [] []
                 ) ;
                 html.text name
               ]
           ] ;
           (if ui.hidden then
              []
            else
              [ ui.fragment.view html ]
           )
         ]
      )
  in
  [ 
    html.div 
      [] 
      [
        Html.onMouseDown 
          html (fun evt -> VDom.stopPropagation evt ; NoOp) ;
        Html.onMouseMove
          html (fun evt -> VDom.stopPropagation evt ; NoOp) ;
        Html.onMouseUp
          html (fun evt -> VDom.stopPropagation evt ; NoOp) ;          
        Html.onMouseClick 
          html (fun evt -> VDom.stopPropagation evt ; NoOp)
      ] 
      (
        List.concat
          [
            (state.sections 
             |> Map.toList 
             |> List.map (fun (n,ui) -> viewComponent html n ui)
            )
          ]
      )
  ]

let view (html : Msg Html) state =
  let controlClass =
    if state.full then
      "control-open"
    else
      "control-closed"
  in
  let toggleClass =
    if state.full then
      "control-toggle"
    else
      "control-toggle control-toggle-shadow"
  in
  let controlView = 
    fun h s -> List.concat [ rootView h s ; panelView h s ]
  in
  [
    html.div
      [ html.className toggleClass ]
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
          (List.concat 
             [ 
               controlView html state ;
               [ panelDisplayHierRow html state state.root ]
             ]
          )
      ]
  ]

let takeUpdate state =
  (state.focused, { state with dirtyPanel = false })
