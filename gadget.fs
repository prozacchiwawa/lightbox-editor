module Gadget

open VDom
open Panel

type Msg =
  | NoOp
  | GadgetMsg of MouseEvent

type Gadget =
  {
    panel : Panel.Panel
  }

let create panel = { panel = panel }
                        
let update msg state =
  state

let view html state =
  html.div [] [] [html.text "Gadget"]

