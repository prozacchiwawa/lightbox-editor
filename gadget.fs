module Gadget

open Html
open Input
open SerializeData

type Styles = (string * string) list

type Msg =
  | NoOp
  | InputMsg of Input.Msg

type ('p,'r) Gadget =
  abstract member name : unit -> string
  abstract member childStyles : int -> 'p -> Styles
  abstract member parentStyles : 'p -> Styles
  abstract member modify : 'p -> 'p
  abstract member view : Msg Html -> 'p -> VDom.VNode
  abstract member update : Msg -> ('p,'r) Gadget
  abstract member serialize : 'p -> SerializeData.Subkey
