module Measure

open Fable.Core
open VDom

type MeasureBounds =
  {
    x : float ;
    y : float ;
    width : float ;
    height : float ;
  }

type MeasurePanel =
  {
    ty: string;
    key: string;
    bounds: MeasureBounds;
    children: MeasurePanel array;
  }

type MeasureMsg =
  {
    ty: string;
    scrollTop: float;
    data: MeasurePanel
  }

[<Emit("$0")>]
let toMeasure : Message -> MeasureMsg = fun m -> failwith "JS only"

let emptyMeasure =
  {
    ty = "measure" ;
    scrollTop = 0. ;
    data =
      {
        ty = "panel" ;
        key = "root" ;
        bounds =
          {
            x = 0. ;
            y = 0. ;
            width = 1000. ;
            height = 1000. ;
          } ;
        children = [| |]
      } ;
  }

type RenderAttribute =
  {
    name : string ;
    value : string ;
  }

type RenderMsg =
  {
    ty : string ;
    tag : string ;
    attributes : RenderAttribute list ;
    xmlns : string option ;
    key : string ;
    children : RenderMsg list ;
  }
