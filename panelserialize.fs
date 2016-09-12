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
      ("layout", panel.layout.serialize panel)
    ]

let rec load ser =
  let layout = LayoutMgrImpl.FlexLayoutMgr(FlexColumn) :> LayoutMgr<Panel,RenderMsg> in
  let emptyPanel =
    {
      id = "" ;
      text = "" ;
      background = "" ;
      children = [] ;
      dummyChildren = [ dummy layout ];
      useWidth = Unspecified ;
      width = 0.0 ;
      useHeight = Unspecified ;
      height = 0.0 ;
      layout = layout
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
         | ("layout",lm) ->
            { panel with layout = LayoutMgrLoad.load lm }
         | _ -> panel)
       emptyPanel
       d
  | _ -> emptyPanel

