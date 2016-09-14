module LayoutMgr

open SerializeData

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
  abstract member childStyles : int -> 'p -> Styles
  abstract member parentStyles : 'p -> Styles
  abstract member update : Msg -> ('p,'r) LayoutMgr
  abstract member serialize : 'p -> SerializeData.Subkey
