module Controls

open Html

type Msg =
  | NoOp
  | ToggleControls

type UI =
  {
    full : bool ;
  }
    
let update action state =
  match action with
  | ToggleControls ->
    { state with full = not state.full }
  | _ -> state

let view (html : Msg Html) state =
  let controlClass =
    if state.full then
      "control-open"
    else
      "control-closed"
  in
  [
    html.div
      [ {name = "className"; value = "control-toggle"} ]
      [ Html.onMouseClick html (fun evt -> ToggleControls) ]
      [ 
        html.i
          [ {name = "className"; value = "fa fa-bars"} ;
            {name = "aria-hidden"; value = "true"} ] [] [] ;
      ] ;
    html.div
      [ {name = "className"; value = controlClass} ] [] []
  ]
