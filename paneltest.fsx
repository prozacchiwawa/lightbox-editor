#r "node_modules/fable-core/Fable.Core.dll"

open Fable.Core
open Fable.Import.Browser

#load "util.fs"
#load "domunits.fs"
#load "dom.fs"
#load "point.fs"
#load "grid.fs"
#load "vdom.fs"
#load "html.fs"
#load "stopmouse.fs"
#load "serializedata.fs"
#load "css.fs"
#load "gridrenderer.fs"
#load "measure.fs"
#load "input.fs"
#load "gadget.fs"
#load "panel.fs"
#load "gadgetimpl.fs"
#load "measurerender.fs"
#load "wireframe.fs"
#load "controls.fs"
#load "q.fs"
#load "storage.fs"
#load "localstorage.fs"
#load "timer.fs"
#load "serialize.fs"
#load "gadgetload.fs"
#load "panelserialize.fs"
#load "toolbox.fs"
#load "dragcontroller.fs"

open Util
open SerializeData
open Serialize

let encodedPanels =
  "{\"id\":\"root\",\"children\":[{\"id\":\"H1RPtM16\",\"children\":[{\"id\":\"rybFtGJT\",\"children\":[],\"layout\":[{\"type\":\"TextGadget\",\"text\":\"Lorem ipsum dolor sit amet\"}]},{\"id\":\"H1gbtFM1T\",\"children\":[],\"layout\":[{\"type\":\"TextGadget\",\"text\":\"Lorem ipsum dolor sit amet\"}]}],\"layout\":[{\"type\":\"SidebarGadget\",\"split\":100}]},{\"id\":\"r1xCvFfJT\",\"children\":[],\"layout\":[{\"type\":\"TextGadget\",\"text\":\"Lorem ipsum dolor sit amet\"}]}],\"layout\":[{\"type\":\"HeaderGadget\",\"split\":50}]}"

let main _ =
  let parsed = encodedPanels |> parse (fun x -> SerializeData.map [("error", String x)] |> subkeyToJson) in
  let asjson = parsed |> Util.expose "asjson" |> jsonToSubkey in
  let decoded = PanelSerialize.load asjson in
  let encoded = PanelSerialize.save decoded in
  let stringified = encoded |> subkeyToJson |> (fun json -> stringify json) in
  begin
    Util.expose "reencoded" stringified ;
    Util.expose "Same" (stringified = encodedPanels)
  end
