module CSS

open Util

let pixelPos v =
  String.concat "" [Util.toString v; "px"]
