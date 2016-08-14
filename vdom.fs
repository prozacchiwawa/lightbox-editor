module VDom

open Fable.Core

type VNode = Unused0
type Event = Unused1
type MouseEvent = { pageX : float; pageY : float }

[<Emit("$0")>]
let toMouseEvent : Event -> MouseEvent = fun e -> failwith "JS only"

type Property = { name : string; value : string }
type Response = { name : string; response : Event -> unit }

type 'msg MsgStream = Unused2

type 'msg VDom =
    { vnode  : string -> Property list -> Response list -> VNode list -> VNode;
      vtext  : string -> VNode;
      post   : 'msg MsgStream -> 'msg -> unit;
      stream : 'msg MsgStream;
    }

type ('init, 'msg, 'state) Program = 
    { init : 'init -> 'state;
      update : 'msg -> 'state -> 'state;
      view : 'state -> VNode
    }
