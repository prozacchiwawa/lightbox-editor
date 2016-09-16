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

let load serialized =
  match serialized with
  | Dict d ->
     (match Map.tryFind "type" d with
      | Some (String "FlexGadget") ->
         loadFlexGadget d :> Gadget<Panel,RenderMsg>
      | _ (*Some (String "TextGadget")*) -> 
         loadTextGadget d :> Gadget<Panel,RenderMsg>
     )
  | _ -> new TextGadget("", None) :> Gadget<Panel,RenderMsg>
