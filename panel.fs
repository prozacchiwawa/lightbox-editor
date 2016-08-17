module Panel

open Point

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
  match p.position with
  | Relative -> "relative"
  | Absolute -> "absolute"

let rec replace p r =
  if r.id = p.id then
    p
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

