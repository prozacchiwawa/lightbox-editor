module DOM

open Fable.Core

open Util
open DomUnits

[<Emit("{ var el = document.getElementById($0); var rect = el ? el.getBoundingClientRect() : null; return rect; }")>]
let getBoundingClientRect : string -> TextRectangle = fun id -> failwith "JS"
