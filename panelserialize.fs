module PanelSerialize

open DomUnits
open LayoutMgr
open Panel
open SerializeData
open LayoutMgrImpl
open LayoutMgrLoad
open Measure

let rec save panel =
  map
    [ ("id", string panel.id) ;
      ("text", string panel.text) ;
      ("background", string panel.background) ;
      ("children", list (List.map save panel.children)) ;
      ("layout", list (List.map (fun (l : LayoutMgr<Panel,RenderMsg>) -> l.serialize panel) panel.layout))
    ]

let rec load ser =
  let emptyPanel =
    {
      id = "" ;
      text = "" ;
      background = "" ;
      children = [] ;
      dummyChildren = [ dummy ];
      useWidth = Unspecified ;
      width = 0.0 ;
      useHeight = Unspecified ;
      height = 0.0 ;
      layout = []
    }
  in
  match ser with
  | Dict d ->
     Map.fold
       (fun panel key v ->
         match (key,v) with
         | ("id",String id) ->
            { panel with id = id }
         | ("text",String text) ->
            { panel with text = text }
         | ("background",String bkg) ->
            { panel with background = bkg }
         | ("children",List l) ->
            { panel with children = List.map load l }
         | ("layout",List l) ->
            { panel with layout = List.map LayoutMgrLoad.load l }
         | _ -> panel)
       emptyPanel
       d
  | _ -> emptyPanel

