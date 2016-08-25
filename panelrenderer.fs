module PanelRenderer

open Util
open Point
open Grid
open VDom
open Html
open CSS
open GridRenderer
open Panel

let view (html : 'a Html.Html) drag grid selected root =
  html.div
    [html.className "root-container"] []
    [
      html.div
        (List.concat [ [html.className "root"]; backgroundSpecification ])
        []
        [ Panel.view (Html.map (fun msg -> PanelMsg msg) html) selected.id root 
        ] ;
      html.div
        [html.className "dragger-container"]
        [
          Html.onMouseDown 
            html 
            (fun evt -> 
              begin
                VDom.preventDefault evt ;
                MouseDown (evt.pageX, evt.pageY)
              end) ;
          Html.onMouseUp 
            html 
            (fun evt -> 
              begin
                VDom.preventDefault evt ;
                MouseUp (evt.pageX, evt.pageY)
              end) ;
          Html.onMouseMove 
            html 
            (fun evt -> 
              begin
                VDom.preventDefault evt ;
                MouseMove (evt.pageX, evt.pageY)
              end)
        ]
        (List.concat
           [
             drag ;
             grid ;
           ]
        )
    ] ;

