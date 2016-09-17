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

let load serialized =
  let cast gadget = gadget :> Gadget<Panel,RenderMsg> in 
  match serialized with
  | Dict d ->
     (match Map.tryFind "type" d with
      | Some (String "FlexGadget") ->
         loadFlexGadget d |> cast
      | Some (String "Sidebar") ->
         loadSidebarGadget d |> cast
      | _ (*Some (String "TextGadget")*) -> 
         loadTextGadget d |> cast
     )
  | _ -> new TextGadget("", None) :> Gadget<Panel,RenderMsg>
