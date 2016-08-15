module Html

open VDom

type 'Msg Html =
    { div : 
          VDom.Property list -> 
          VDom.Response list -> 
          VDom.VNode list ->
          VDom.VNode ;
      button : 
          VDom.Property list -> 
          VDom.Response list -> 
          VDom.VNode list ->
          VDom.VNode ;
      input :
          VDom.Property list ->
          VDom.Response list ->
          VDom.VNode list ->
          VDom.VNode ;
      i :
          VDom.Property list ->
          VDom.Response list ->
          VDom.VNode list ->
          VDom.VNode ;
      text : string -> VDom.VNode ;
      style : (string * string) list -> VDom.Property;
      onMouseDown : (MouseEvent -> 'Msg) -> VDom.Response;
      onMouseUp : (MouseEvent -> 'Msg) -> VDom.Response;
      onMouseMove : (MouseEvent -> 'Msg) -> VDom.Response;
      onMouseClick : (MouseEvent -> 'Msg) -> VDom.Response;
    }

let makeCSS l =
    String.concat ";" (List.map (fun (n,v) -> String.concat ": " [n;v]) l)

let onMouseDown vdom f = 
  { name = "mousedown" ; 
    response = fun evt -> vdom.post vdom.stream (f (VDom.toMouseEvent evt)) 
  }
let onMouseUp vdom f = 
  { name = "mouseup" ; 
    response = fun evt -> vdom.post vdom.stream (f (VDom.toMouseEvent evt)) 
  }
let onMouseMove vdom f = 
  { name = "mousemove" ;
    response = fun evt -> vdom.post vdom.stream (f (VDom.toMouseEvent evt))
  }
let onMouseClick vdom f = 
  { name = "click" ; 
    response = fun evt -> vdom.post vdom.stream (f (VDom.toMouseEvent evt)) 
  }

let html vdom =
    { div = vdom.vnode "div" ;
      button = vdom.vnode "button" ;
      input = vdom.vnode "input" ;
      i = vdom.vnode "i" ;
      text = vdom.vtext ;
      style = fun l -> {name = "style"; value = makeCSS l} ;
      onMouseDown = onMouseDown vdom ;
      onMouseUp = onMouseUp vdom ;
      onMouseMove = onMouseMove vdom ;
      onMouseClick = onMouseClick vdom
    }
