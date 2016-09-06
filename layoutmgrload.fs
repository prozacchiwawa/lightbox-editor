module LayoutMgrLoad

open SerializeData
open Panel
open LayoutMgr
open LayoutMgrImpl
open Measure

type Panel = Panel.Panel
type RenderMsg = Measure.RenderMsg

let load serialized =
  match serialized with
  | Dict d ->
     (match Map.tryFind "type" d with
      | Some _ -> new FreeLayoutMgr() :> LayoutMgr<Panel,RenderMsg>
     )
  | _ -> new FreeLayoutMgr() :> LayoutMgr<Panel,RenderMsg>
