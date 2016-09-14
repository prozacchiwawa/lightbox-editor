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
  | ChangeBackground of string
  | ChangePanel of Panel
  | SelectPanel of string
  | TogglePanel of (string * bool)
  | SetGrid of Grid
  | DeletePanel of string
  | EditorMsg of (string * string * Input.Msg)

type UISectionContainer =
  {
    name : string ;
    hidden : bool ;
  }

type EditPane =
  {
    name : string ;
    editors : Input.EditorSet
  }

type UI =
  {
    full : bool ;
    backgroundUrl : string ;
    root : Panel ;
    focused : Panel ;
    dirtyPanel : bool ;
    openPanels : Set<string> ;
    panelEditors : Map<string, EditPane> ;
    grid : Grid
  }

let init grid panel = 
  { full = false ; 
    backgroundUrl = "" ;
    root = panel ;
    focused = panel ; 
    dirtyPanel = false ;
    openPanels = Set<string> [] ;
    panelEditors = Map<string, EditPane> [] ;
    grid = grid
  }

let select panel root state =
  let _ = Util.log "Controls.select with" (panel.id, root.id) in
  { state with 
    focused = panel ; 
    root = root 
  }

let dropEditor state pid =
  Map.remove pid state.panelEditors

let createEditors state panel =
  state.panelEditors

let update action state =
  match action with
  | ToggleControls ->
     { state with full = not state.full }
  | TogglePanel (pid,openPanel) ->
     { state with 
       openPanels = 
         if openPanel then
           Set.add pid state.openPanels
         else
           Set.remove pid state.openPanels ;
       panelEditors = 
         if openPanel then
           createEditors state pid
         else
           dropEditor state pid
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

let viewControls html state panel =
  [ 
    html.div
      [html.className "SNAAAKE111"] [] []
  ]

let rec panelDisplayHierRow (html : Msg Html) state panel =
  let treeRowClass =
    if state.focused.id = panel.id then
      "panel-tree-row-selected"
    else
      "panel-tree-row"
  in
  let panelOpen = Set.contains panel.id state.openPanels in
  html.div
    [html.className treeRowClass] []
    (List.concat 
       [
         [
           html.i 
             [
               html.className 
                 (if panelOpen then 
                    "fa fa-chevron-down"
                  else 
                    "fa fa-chevron-right"
                 )
             ]
             [
               Html.onMouseClick 
                 html 
                 (fun evt -> 
                   begin
                     VDom.preventDefault evt ;
                     VDom.stopPropagation evt ;
                     TogglePanel (panel.id, not panelOpen)
                   end
                 )
             ] [] ;
           html.div
             [html.className "panel-tree-row-name"]
             [
               Html.onMouseClick 
                 html 
                 (fun evt -> 
                   begin
                     VDom.preventDefault evt ;
                     VDom.stopPropagation evt ;
                     SelectPanel panel.id
                   end
                 )
             ]
             [ html.text (String.concat " " ["Panel";panel.id]) ]
         ];
         (if panelOpen then
            (List.concat
               [
                 viewControls html state panel ;
                 panel.children
                 |> List.rev
                 |> List.map (panelDisplayHierRow html state)
               ]
            )
         else
           [])
       ]
    )

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
               [ panelDisplayHierRow html state state.root ]
             ]
          )
      ]
  ]

let takeUpdate state =
  (state.focused, { state with dirtyPanel = false })
