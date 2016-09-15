module LayoutMgr

open Html
open Input
open SerializeData

type Styles = (string * string) list

type Msg =
  | NoOp
  | InputMsg of Input.Msg

type ('p,'r) LayoutMgr =
  abstract member childStyles : int -> 'p -> Styles
  abstract member parentStyles : 'p -> Styles
  abstract member view : Msg Html -> 'p -> VDom.VNode
  abstract member update : Msg -> ('p,'r) LayoutMgr
  abstract member serialize : 'p -> SerializeData.Subkey
