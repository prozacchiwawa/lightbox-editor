module VDom

open Fable.Core
open Point

type Point = Point.Point

type VNode = Unused0
type Event = Unused1
type Element = Unused2
type MouseEvent = { pageX : float; pageY : float; target : Element }
type InputEventTarget = { value : string; }
type InputEvent = { target : InputEventTarget }
type SelectEventTarget = { selectedIndex : int }
type SelectEvent = { target : SelectEventTarget }

[<Emit("$0")>]
let toMouseEvent : Event -> MouseEvent = fun e -> failwith "JS only"

[<Emit("($0).preventDefault();")>]
let preventDefault : 'a -> unit = fun e -> failwith "JS only"

[<Emit("($0).stopPropagation();")>]
let stopPropagation : 'a -> unit = fun e -> failwith "JS only"

[<Emit("$0")>]
let toInputEvent : Event -> InputEvent = fun e -> failwith "JS only"

[<Emit("$0")>]
let toSelectEvent : Event -> SelectEvent = fun e -> failwith "JS only"

[<Emit("$0")>]
let elementDimensions : Element -> (Point * Point) = fun e -> failwith "JS only"

type Property = { name : string; value : string }
type Response = { name : string; response : Event -> unit }

type 'msg MsgStream = Unused3

type 'msg VDom =
    { vnode  : string -> Property list -> Response list -> VNode list -> VNode;
      vtext  : string -> VNode;
      post   : 'msg -> unit;
    }

type ('init, 'msg, 'state) Program = 
    { init : 'init -> 'state;
      update : 'msg -> 'state -> 'state;
      view : 'state -> VNode
    }

let map f vdom =
  { post = fun msg -> vdom.post (f msg) ;
    vnode = vdom.vnode ;
    vtext = vdom.vtext ;
  }


type Tx1 = Ux1
type Tx2 = Ux2

let t2ToT1 t2 = Ux1

let (vT1 : Tx1 VDom) = 
  { post = fun (t : Tx1) -> () ; 
    vnode = fun _ _ _ _ -> Unused0 ; 
    vtext = fun _ -> Unused0 
  }

let (vT2 : Tx2 VDom) = map t2ToT1 vT1

