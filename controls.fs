module Controls

open Html
open Panel

type Panel = Panel.Panel

type Msg =
  | NoOp
  | ToggleControls
  | BackgroundInput of string
  | ChangeBackground of string
  | ChangePanel of Panel

type UI =
  {
    full : bool ;
    backgroundUrl : string ;
    focused : Panel ;
  }

let init panel = { full = false ; backgroundUrl = "" ; focused = panel }

let select panel state =
  { state with focused = panel }
    
let update action state =
  match action with
  | ToggleControls ->
    { state with full = not state.full }
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

let panelView (html : Msg Html) state =
  [ 
    html.div 
      [] [] 
      [ html.text (String.concat " " ["Panel"; state.focused.id]) ]
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
