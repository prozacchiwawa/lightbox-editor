module DomUnits

type TextRectangle =
  {
    left : float ;
    top : float ;
    width : float ; 
    height : float ;
  }

type DimUnit =
  | Unspecified
  | Px
  | Vmin
  | Vh
  | Vw
  | Percent

type BorderUseDim =
  | Linked
  | Left of (DimUnit * float)
  | Right of (DimUnit * float)
  | Top of (DimUnit * float)
  | Bottom of (DimUnit * float)

let cssDimName du =
  match du with
  | Unspecified -> None
  | Px -> Some "px"
  | Vmin -> Some "vmin"
  | Vh -> Some "vh"
  | Vw -> Some "vw"
  | Percent -> Some "%"

let cssDim du v =
  du
  |> cssDimName
  |> Util.maybeMap (fun u -> String.concat "" [Util.toString v; u])
