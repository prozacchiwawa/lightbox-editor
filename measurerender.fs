module MeasureRender

open DomUnits
open VDom
open Panel
open Measure

type MR = Measure.RenderMsg

let rec render styles' children parent panel =
  let width = 
    cssDim panel.useWidth panel.width
    |> Util.maybeMap (fun v -> ("width", v))
    |> Util.maybeSingleton
  in
  let height =
    cssDim panel.useHeight panel.height
    |> Util.maybeMap (fun v -> ("height", v))
    |> Util.maybeSingleton
  in
  let styles = 
    List.concat [ styles' ; [ ("background-color", panel.background) ] ]
  in
  { MR.ty = "render" ;
    MR.tag = "div" ;
    MR.key = panel.id ;
    MR.attributes = [{name = "style" ; value = String.concat ";" (List.map (fun (n,v) -> String.concat ":" [n;v]) styles)}] ;
    MR.xmlns = None ;
    MR.text = panel.text ;
    MR.children =
      match (panel.text, children) with
      | ("", []) -> List.map (render [ ("padding", "30px") ] [] panel) panel.dummyChildren
      | _ -> children
  }
