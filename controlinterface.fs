module ControlInterface

open VDom
open Html

type ('edit,'msg) ControlInterface =
  abstract member view : 'msg Html -> VDom.VNode
  abstract member update : 'msg -> ('edit,'msg) ControlInterface
  abstract member dirty : unit -> bool
  abstract member take : unit -> ('edit * ('edit,'msg) ControlInterface)
