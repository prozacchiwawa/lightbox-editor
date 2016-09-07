#r "node_modules/fable-core/Fable.Core.dll"

open Fable.Core
open Fable.Import.Browser

#load "util.fs"
#load "point.fs"
#load "vdom.fs"
#load "html.fs"
#load "dragcontroller.fs"

open Point
open DragController

type Msg =
  | NoOp

type Draggable =
  {
    at : Point
  }

type State =
  {
    dragger : (Draggable, State, Msg) DragController.State ;
    objects : Draggable list
  }

let init arg =
  {
    dragger = emptyDragController NoOp ;
    objects = []
  }

let update msg state =
  state

let view (html : Msg Html.Html) state =
  html.div [] [] [html.text "Test"]

let main (vdom : Msg VDom.VDom) arg =
  let post : Msg -> unit = vdom.post in
  { VDom.init = init ;
    VDom.update = (fun msg state -> update msg state) ;
    VDom.view = (view (Html.html vdom)) ;
  }
