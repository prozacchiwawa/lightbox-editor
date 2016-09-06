module SerializeData

type Subkey =
  | Null
  | String of string
  | Float of float
  | Bool of bool
  | List of Subkey list
  | Dict of Map<string, Subkey>

let map subkeys = Dict (Map<string, Subkey> subkeys)
let string str = String str
let num flt = Float flt
let bool b = Bool b
let jsnull u = Null
let list l = List l
let dict d = Dict d
