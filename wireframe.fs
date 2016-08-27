module Wireframe

open Util
open Html
open Panel
open Measure

let view 
      (html : 'msg Html.Html) 
      (selected : string) 
      (measureRoot : Measure.MeasureMsg) =
  let rec viewPanel html (measure : Measure.MeasurePanel) =
    let panelClass = 
      if selected = measure.key then
        String.concat " " ["panel";"panel-selected"]
      else 
        "panel" 
    in
    let toPx p = String.concat "" [Util.toString p; "px"] in
    let styles = 
      [
        ("left", toPx measure.bounds.x);
        ("top", toPx measure.bounds.y);
        ("width", toPx measure.bounds.width);
        ("height", toPx measure.bounds.height)
      ]
    in
    html.div
      [
        html.className panelClass ;
        html.style styles 
      ]
      []
      (List.concat 
         [
           [
             html.div
               [html.className "panel-hide-layout"]
               []
               [html.text (String.concat " " ["PANEL:";measure.key])]
           ];
           (List.map (viewPanel html) (Array.toList measure.children))
         ]
      )
  in
  viewPanel html measureRoot.data
