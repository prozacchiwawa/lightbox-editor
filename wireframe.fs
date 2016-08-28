module Wireframe

open Util
open Html
open Panel
open Measure

let view 
      (html : 'msg Html.Html) 
      (selected : string) 
      (measureRoot : Measure.MeasureMsg) =
  let rec viewPanel 
        html 
        (parent : Measure.MeasurePanel) 
        (measure : Measure.MeasurePanel) =
    let panelClass = 
      if selected = measure.key then
        String.concat " " ["panel";"panel-selected"]
      else 
        "panel" 
    in
    let toPx p = String.concat "" [Util.toString p; "px"] in
    let styles = 
      [
        ("left", toPx (measure.bounds.x - parent.bounds.x - 1.));
        ("top", toPx (measure.bounds.y - parent.bounds.y - 1.));
        ("width", toPx (measure.bounds.width + 2.));
        ("height", toPx (measure.bounds.height + 2.))
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
           (List.map 
              (viewPanel html measure)
              (Array.toList measure.children)
           )
         ]
      )
  in
  viewPanel html measureRoot.data measureRoot.data
