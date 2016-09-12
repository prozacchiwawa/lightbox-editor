module Panel

open Util
open DomUnits
open DOM
open Point
open CSS
open VDom
open Html
open LayoutMgr
open Measure

type Panel =
  {
    id : string ;
    text : string ;
    background : string ;
    children : Panel list ;
    useWidth : DimUnit ;
    useHeight : DimUnit ;
    height : float ;
    width : float ;
    dummyChildren : Panel list ;
    layout : LayoutMgr.LayoutMgr<Panel,Measure.RenderMsg>
  }
       
let dummy layout =
  { id = "" ;
    text = "Dummy content" ;
    background = "" ;
    children = [] ;
    dummyChildren = [] ;
    useWidth = Px ;
    width = 45.0 ;
    useHeight = Px ;
    height = 300.0 ;
    layout = layout
  }

let rec fromId id panel =
  let matchingChildPanels =
    List.concat (List.map (fromId id) panel.children)
  in
  let matchingThisPanel =
    if id = panel.id then
      [ panel ]
    else 
      []
  in
  List.concat [matchingChildPanels; matchingThisPanel]

let parent id parent_ panel =
  let rec treeWithParents parent_ panel =
    seq 
      {
        yield (parent_,panel) ;
        yield! 
             (panel.children 
              |> Seq.map (treeWithParents panel) 
              |> Seq.concat)
      }
  in
  let parentsAndChildren = (treeWithParents parent_ panel) in
  parentsAndChildren
  |> Seq.skipWhile (fun (parent,p) -> p.id <> id)
  |> Seq.take 1
  |> Seq.toList
  |> Util.expose "parent"
  |> List.map (fun (parent,p) -> parent)

let rec replace p r =
  if p.id <> r.id then
    { r with children = List.map (replace p) r.children }
  else
    p

let rec remove pid root =
  { root with 
    children = 
      List.map 
        (remove pid)
        (List.filter (fun p -> p.id <> pid) root.children)
  }
