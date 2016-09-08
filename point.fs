module Point

open Util

type Point = { x : float; y : float }

let inside a b c =
  (Util.between a.x b.x c.x) && (Util.between a.y b.y c.y)

let subtract a b =
  { x = a.x - b.x; y = a.y - b.y }

let add a b =
  { x = a.x + b.x; y = a.y + b.y }

let ctor x y = { x = x ; y = y }

let inDistBox n a b =
  (abs (b.x - a.x)) < n && (abs (b.y - a.y)) < n

  
