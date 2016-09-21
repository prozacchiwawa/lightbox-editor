module PanelSerialize

open DomUnits
open Gadget
open Panel
open SerializeData
open GadgetImpl
open GadgetLoad
open Measure

let rec save panel =
  map
    [ ("id", string panel.id) ;
      ("children", list (List.map save panel.children)) ;
      ("layout", list (List.map (fun (l : Gadget<Panel,RenderMsg>) -> l.serialize panel) panel.layout))
    ]

let rec load ser =
  let emptyPanel =
    {
      id = "" ;
      text = "" ;
      children = [] ;
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
         | ("children",List l) ->
            { panel with children = List.map load l }
         | ("layout",List l) ->
            { panel with layout = List.map GadgetLoad.load l }
         | _ -> panel)
       emptyPanel
       d
     |> updateWithGadgets
  | _ -> emptyPanel

