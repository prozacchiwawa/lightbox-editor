module StopMouse

let stopMouse html msg =
  [ 
    Html.onMouseClick
      html (fun evt -> (VDom.stopPropagation evt ; msg)) ;
    Html.onMouseDown
      html (fun evt -> (VDom.stopPropagation evt ; msg)) ;
    Html.onMouseUp
      html (fun evt -> (VDom.stopPropagation evt ; msg)) ;
    Html.onMouseMove
      html (fun evt -> (VDom.stopPropagation evt ; msg)) ;
  ]
