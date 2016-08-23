module Grid

open Util
open Point

type Grid =
  {
    enabled : bool ;
    offset : Point ;
    interval : Point ;
  }

let create enabled offset interval =
  {
    enabled = enabled ;
    offset = offset ;
    interval = interval ;
  }

let snap point grid =
  if grid.enabled then
    let at = Point.subtract point grid.offset in
    Point.ctor 
      (Util.snap at.x grid.interval.x) 
      (Util.snap at.y grid.interval.y)
  else
    point
