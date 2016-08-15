module Html

open VDom

type 'Msg Html =
    { vdom : 'Msg VDom.VDom ;
      div : 
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
    }

let makeCSS l =
    String.concat ";" (List.map (fun (n,v) -> String.concat ": " [n;v]) l)

let onMouseDown html f = 
  let vdom = html.vdom in
  { name = "mousedown" ; 
    response = fun evt -> vdom.post (f (VDom.toMouseEvent evt)) 
  }
let onMouseUp html f = 
  let vdom = html.vdom in
  { name = "mouseup" ; 
    response = fun evt -> vdom.post (f (VDom.toMouseEvent evt)) 
  }
let onMouseMove html f = 
  let vdom = html.vdom in
  { name = "mousemove" ;
    response = fun evt -> vdom.post (f (VDom.toMouseEvent evt))
  }
let onMouseClick html f = 
  let vdom = html.vdom in
  { name = "click" ; 
    response = fun evt -> vdom.post (f (VDom.toMouseEvent evt)) 
  }

let html vdom =
  { vdom = vdom ;
    div = vdom.vnode "div" ;
    button = vdom.vnode "button" ;
    input = vdom.vnode "input" ;
    i = vdom.vnode "i" ;
    text = vdom.vtext ;
    style = fun l -> {name = "style"; value = makeCSS l} ;
  }

let map f someHtml =
  html (VDom.map f someHtml.vdom)
