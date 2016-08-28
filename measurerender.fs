module MeasureRender

open VDom
open Panel
open Measure

type MR = Measure.RenderMsg

let render styles children parent panel =
  { MR.ty = "render" ;
    MR.tag = "div" ;
    MR.key = panel.id ;
    MR.attributes = [{name = "style" ; value = String.concat ";" (List.map (fun (n,v) -> String.concat ":" [n;v]) styles)}] ;
    MR.xmlns = None ;
    MR.children = children
  }
