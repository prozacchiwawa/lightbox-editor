module Measure

open Fable.Core
open Point
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
    text : string ;
    children : RenderMsg list ;
  }

let upperLeft measure =
  Point.ctor measure.bounds.x measure.bounds.y

let lowerRight measure =
  Point.ctor 
    (measure.bounds.x + measure.bounds.width) 
    (measure.bounds.y + measure.bounds.height)

let pointInside coords measure =
  let ul = upperLeft measure in
  let lr = lowerRight measure in
  Point.inside ul lr coords

let rec fromCoord coords measure =
  let ul = upperLeft measure in
  let matchingChildPanels =
    List.concat (List.map (fromCoord (Point.subtract coords ul)) (Array.toList measure.children))
  in
  let matchingThisPanel =
    if pointInside coords measure then
      [ measure ]
    else 
      []
  in
  List.concat [matchingChildPanels; matchingThisPanel]

