module LayoutMgr

type Msg =
  | NoOp
  | MouseDown of float * float
  | MouseMove of float * float
  | MouseUp of float * float

type Styles = (string * string) list

type ('p,'r) RenderPanel =
  (string * string) list -> 'r list -> 'p -> 'p -> 'r

let cssFromStyles styles moreStyles =
  List.concat [moreStyles ; styles]

type ('p,'r) LayoutMgr =
  abstract member view : 
    Styles -> 
    ('p -> ('p,'r) LayoutMgr) ->
    ('p,'r) RenderPanel -> 
    'p ->
    'p -> 
    'r
  abstract member childStyles : int -> 'p -> Styles
  abstract member parentStyles : 'p -> Styles
  abstract member update : Msg -> ('p,'r) LayoutMgr
