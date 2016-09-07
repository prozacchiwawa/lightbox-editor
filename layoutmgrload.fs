module LayoutMgrLoad

open SerializeData
open Panel
open LayoutMgr
open LayoutMgrImpl
open Measure

type Panel = Panel.Panel
type RenderMsg = Measure.RenderMsg

let loadFlexLayoutMgr d =
  match Map.tryFind "flex-direction" d with
  | Some (String dir) -> new FlexLayoutMgr(flexDirectionOfString dir)
  | None -> new FlexLayoutMgr(FlexColumn)

let load serialized =
  match serialized with
  | Dict d ->
     (match Map.tryFind "type" d with
      | Some _ -> loadFlexLayoutMgr d :> LayoutMgr<Panel,RenderMsg>
     )
  | _ -> new FlexLayoutMgr(FlexColumn) :> LayoutMgr<Panel,RenderMsg>
