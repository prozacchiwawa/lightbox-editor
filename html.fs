module Html

open VDom
open Util

type 'Msg Svg =
    { vdom : 'Msg VDom.VDom ;
      root : 
        VDom.Property list ->
        VDom.VNode list ->
        VDom.VNode ;
      defs : VDom.VNode list -> VDom.VNode ;
      pattern :
        string -> 
        float -> float -> float -> float ->
        VDom.VNode list ->
        VDom.VNode ;
      line : VDom.Property list -> VDom.VNode ;
      rect :
        VDom.Property list ->
        VDom.Response list ->
        VDom.VNode list ->
        VDom.VNode 
    }

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
      select : 
          VDom.Property list ->
          VDom.Response list ->
          VDom.VNode list ->
          VDom.VNode ;
      option :
          VDom.Property list ->
          VDom.Response list ->
          VDom.VNode list ->
          VDom.VNode ;
      iframe :
          VDom.Property list ->
          VDom.Response list ->
          VDom.VNode list ->
          VDom.VNode ;
      svg : 'Msg Svg ;
      text : string -> VDom.VNode ;
      attribute : string -> string -> VDom.Property;
      style : (string * string) list -> VDom.Property;
      className : string -> VDom.Property;
      inputValue : string -> VDom.Property;
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
let onInput html f =
  let vdom = html.vdom in
  { name = "input" ;
    response = fun evt -> vdom.post (f (VDom.toInputEvent evt))
  }
let onSelectChange html f =
  let vdom = html.vdom in
  { name = "change" ;
    response = fun evt -> vdom.post (f (VDom.toSelectEvent evt))
  }
    
let html vdom =
  { vdom = vdom ;
    div = vdom.vnode "div" ;
    button = vdom.vnode "button" ;
    input = vdom.vnode "input" ;
    i = vdom.vnode "i" ;
    select = vdom.vnode "select" ;
    option = vdom.vnode "option" ;
    iframe = vdom.vnode "iframe" ;
    svg =
      { vdom = vdom ;
        root = 
          (fun attrs children ->
            vdom.vnode
              "svg"
              (List.concat 
                 [ 
                   [ 
                     {name = "xmlns"; value = "http://www.w3.org/2000/svg"} ;
                     {name = "version"; value = "1.1"}
                   ] ;
                   attrs
                 ]
              )
              []
              children
          ) ;
        defs = 
          (fun children -> 
            vdom.vnode 
              "defs" 
              [{name = "xmlns"; value = "http://www.w3.org/2000/svg"}] 
              [] children
          ) ;
        pattern =
          (fun id x y w h children -> 
            vdom.vnode 
              "pattern" 
              [
                {name = "xmlns"; value = "http://www.w3.org/2000/svg"} ;
                {name = "id" ; value = id} ;
                {name = "x" ; value = Util.toString x} ;
                {name = "y" ; value = Util.toString y} ;
                {name = "width" ; value = Util.toString w} ;
                {name = "height" ; value = Util.toString h} ;
                {name = "patternUnits" ; value = "userSpaceOnUse"}
              ] 
              [] children
          ) ;
        line =
          (fun props -> 
            vdom.vnode 
              "line" 
              (List.concat 
                 [ 
                   [{name = "xmlns"; value = "http://www.w3.org/2000/svg"}] ;
                   props
                 ]
              ) [] []
          ) ;
        rect =
          (fun props responders children ->
            vdom.vnode 
              "rect" 
              (List.concat 
                 [ 
                   [{name="xmlns"; value="http://www.w3.org/2000/svg"}] ;
                   props
                 ]
              ) responders children
          )
      }
    text = vdom.vtext ;
    attribute = fun n v -> { name = n; value = v } ;
    style = fun l -> {name = "style"; value = makeCSS l} ;
    className = fun c -> {name = "class"; value = c} ;
    inputValue = fun v -> {name = "value"; value = v} ;
  }

let map f someHtml =
  html (VDom.map f someHtml.vdom)
