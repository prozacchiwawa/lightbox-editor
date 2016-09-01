module Storage

open Q

type Storage =
  abstract member get : string -> string Q.Promise
  abstract member set : string -> string -> unit Q.Promise
