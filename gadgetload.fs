module GadgetLoad

open SerializeData
open Panel
open Gadget
open GadgetImpl
open Measure

type Panel = Panel.Panel
type RenderMsg = Measure.RenderMsg

let loadFlexGadget d =
  match Map.tryFind "flex-direction" d with
  | Some (String dir) -> new FlexGadget(flexDirectionOfString dir, None)
  | None -> new FlexGadget(FlexColumn, None)

let loadTextGadget d =
  match Map.tryFind "text" d with
  | Some (String text) -> new TextGadget(text, None)
  | None -> new TextGadget("", None)

let loadSidebarGadget d =
  match Map.tryFind "split" d with
  | Some (Float f) -> new SidebarGadget(f, None)
  | None -> new SidebarGadget(100.0, None)

let loadHeaderGadget d =
  match Map.tryFind "split" d with
  | Some (Float f) -> new HeaderGadget(f, None)
  | None -> new HeaderGadget(50.0, None)

let load serialized =
  let cast gadget = gadget :> Gadget<Panel,RenderMsg> in 
  match serialized with
  | Dict d ->
     let typeOpt = Map.tryFind "type" d in
     (match typeOpt with
      | Some (String "FlexGadget") ->
         loadFlexGadget d |> cast
      | Some (String "SidebarGadget") ->
         loadSidebarGadget d |> cast
      | Some (String "HeaderGadget") ->
         loadHeaderGadget d |> cast
      | _ (*Some (String "TextGadget")*) -> 
         loadTextGadget d |> cast
     )
  | _ -> new TextGadget("", None) :> Gadget<Panel,RenderMsg>
