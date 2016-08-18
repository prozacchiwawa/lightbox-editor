module Panel

open Util
open Point
open CSS
open Html

type AxisPosition =
  |   LowGrav of float * float
  |   HighGrav of float * float
  |   MidCover of float * float
                            
type Position =
  |   Absolute
  |   Relative

type Panel =
  {
    lr : AxisPosition ;
    tb : AxisPosition ;
    position : Position ;
    background : string ;
    id : string ;
    children : Panel list ;
  }

type Msg =
  | NoOp

let upperLeft p =
  let l = 
    match p.lr with
    | MidCover (l,r) -> l
    | LowGrav (l,w) -> l
    | HighGrav (w,r) -> (r - w)
  in
  let t =
    match p.tb with
    | MidCover (t,b) -> t
    | LowGrav (t,h) -> t
    | HighGrav (h,b) -> (b - h)
  in 
  Point.ctor l t

let lowerRight p =
  let r =
    match p.lr with
    | MidCover (l,r) -> r
    | LowGrav (l,w) -> (l + w)
    | HighGrav (w,r) -> r
  in
  let b =
    match p.tb with
    | MidCover (t,b) -> b
    | LowGrav (t,h) -> (t + h)
    | HighGrav (h,b) -> b
  in
  Point.ctor r b
       
let pointInside coords panel =
  let ul = upperLeft panel in
  let lr = lowerRight panel in
  Point.inside ul lr coords

let rec offset id root =
  let ul = upperLeft root in
  if root.id = id then
    [ ul ]
  else
    let children = List.concat (List.map (offset id) root.children) in
    List.map (Point.add ul) children

let rec fromCoord coords panel =
  let ul = upperLeft panel in
  let matchingChildPanels =
    List.concat (List.map (fromCoord (Point.subtract coords ul)) panel.children)
  in
  let matchingThisPanel =
    if pointInside coords panel then
      [ panel ]
    else 
      []
  in
  List.concat [matchingChildPanels; matchingThisPanel]

let positionString p =
  match p with
  | Relative -> "relative"
  | Absolute -> "absolute"

let rec updateChildPositions p r =
  let adjustChildPosition diffWidth diffHeight child =
    let p1 =
      match child.lr with
      | MidCover (l,r) ->
         { child with lr = MidCover (l,r + diffWidth) }
      | HighGrav (w,r) ->
         { child with lr = HighGrav (w,r + diffWidth) }
      | _ -> child
    in
    let p2 =
      match p1.tb with
      | MidCover (t,b) ->
         { p1 with tb = MidCover (t,b + diffHeight) }
      | HighGrav (h,b) ->
       { p1 with tb = HighGrav (h,b + diffHeight) }
      | _ -> p1
    in
    updateChildPositions p2 child
  in
  let pUL = upperLeft p in
  let pLR = lowerRight p in
  let rUL = upperLeft r in
  let rLR = lowerRight r in
  let pWidth = pLR.x - pUL.x in
  let pHeight = pLR.y - pUL.y in
  let rWidth = rLR.x - rUL.x in
  let rHeight = rLR.y - rUL.y in
  let diffWidth = Util.log "diffWidth" (pWidth - rWidth) in
  let diffHeight = Util.log "diffHeight" (pHeight - rHeight) in
  { r with 
    children =
      r.children |>
        List.map (adjustChildPosition diffWidth diffHeight)
  }

let rec replace p r =
  if r.id = p.id then
    updateChildPositions p r
  else
    { r with children = List.map (replace p) r.children }

let setLeft l p =
  match p.lr with
  | MidCover (l_,r) -> { p with lr = MidCover (l,r) }
  | LowGrav (l_,w) -> { p with lr = LowGrav (l,(w + l_ - l)) }
  | HighGrav (w,r) -> { p with lr = HighGrav (l - r,r) }

let setRight r p =
  match p.lr with
  | MidCover (l_,r_) -> { p with lr = MidCover (l_,r) }
  | LowGrav (l,w) -> { p with lr = LowGrav (l, r - l) }
  | HighGrav (w,r_) -> { p with lr = HighGrav (w + (r_ - r),r) }

let setTop l p =
  match p.tb with
  | MidCover (l_,r) -> { p with tb = MidCover (l,r) }
  | LowGrav (l_,w) -> { p with tb = LowGrav (l,(w + l_ - l)) }
  | HighGrav (w,r) -> { p with tb = HighGrav (l - r,r) }

let setBottom r p =
  match p.tb with
  | MidCover (l_,r_) -> { p with tb = MidCover (l_,r) }
  | LowGrav (l,w) -> { p with tb = LowGrav (l, r - l) }
  | HighGrav (w,r_) -> { p with tb = HighGrav (w + (r_ - r),r) }

let xAxisPositionString a =
  match a with
  | MidCover _ -> "left <-> right"
  | LowGrav _ -> "left -> width"
  | HighGrav _ -> "width <- right"

let yAxisPositionString a =
  match a with
  | MidCover _ -> "top <-> bottom"
  | LowGrav _ -> "top -> height"
  | HighGrav _ -> "height <- bottom"

let positionList = [ Absolute ; Relative ]
let gravityList = [ MidCover (0.,0.); LowGrav (0.,0.); HighGrav (0.,0.) ]

let stringToPosition s =
  let candidates =
    positionList |>
      List.map (fun p -> (positionString p, p)) |>
      List.filter (fun (ps,p) -> ps = s)
  in
  match candidates with
  | (hs,hp) :: tl -> hp
  | [] -> Absolute

let setPosition pos panel =
  { panel with position = pos }

let xAxisStringToGravity s left right =
  let candidates =
    gravityList |>
      List.map (fun p -> (yAxisPositionString p, p)) |>
      List.filter (fun (ps,p) -> ps = s)
  in
  match candidates with
  | (_,MidCover _) :: tl -> MidCover (left, right)
  | (_,HighGrav _) :: tl -> HighGrav (right - left, right)
  | _ -> LowGrav (left, right - left)

let yAxisStringToGravity s top bottom =
  let candidates =
    gravityList |>
      List.map (fun p -> (yAxisPositionString p, p)) |>
      List.filter (fun (ps,p) -> ps = s)
  in
  match candidates with
  | (_,MidCover _) :: tl -> MidCover (top, bottom)
  | (_,HighGrav _) :: tl -> HighGrav (bottom - top, bottom)
  | _ -> LowGrav (top, bottom - top)

let setLRMeasure lr panel =
  { panel with lr = lr }

let setTBMeasure tb panel =
  { panel with tb = tb }

let view (html : Msg Html.Html) selected panel =
  let rec viewPanel (panel : Panel) =
    let panelClass = 
      if selected = panel.id then 
        String.concat " " ["panel";"panel-selected"]
      else 
        "panel" 
    in
    let draggerUL = upperLeft panel in
    let draggerBR = lowerRight panel in
    html.div
      [
        html.className panelClass ;
        html.style 
          [
            ("position", positionString panel.position);
            ("left", CSS.pixelPos draggerUL.x);
            ("top", CSS.pixelPos draggerUL.y);
            ("width", CSS.pixelPos (draggerBR.x - draggerUL.x));
            ("height", CSS.pixelPos (draggerBR.y - draggerUL.y))
          ]
      ]
      []
      (List.concat 
         [
           [
             html.div
               [html.className "panel-hide-layout"]
               []
               [html.text (String.concat " " ["PANEL:";panel.id])]
           ];
           List.map viewPanel panel.children
         ]
      )
  in
  viewPanel panel
