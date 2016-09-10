module DOM

open Fable.Core

type TextRectangle =
  {
    left : float ;
    top : float ;
    width : float ; 
    height : float ;
  }

[<Emit("{ var el = document.getElementById($0); var rect = el ? el.getBoundingClientRect() : null; return rect; }")>]
let getBoundingClientRect : string -> TextRectangle = fun id -> failwith "JS"

