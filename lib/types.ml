type p =
  | Void
  | Boolean
  | Byte
  | Short
  | Char
  | Int
  | Long
  | Float
  | Double
  [@@deriving show]

type o = {path: string list; cls: string}
  [@@deriving show]
(* FIXME what about internal classes? *)

let lang cls = {
  path = ["java"; "lang"];
  cls;
} [@@deriving show]

let obj = lang "Object"
let string = lang "String"

type v =
  | Primitive of p
  (* reference *)
  | Object of o
  | Array of int * v
  [@@deriving show]

type m = {obj: o; meth: string; params: v list; ret: v;}
  [@@deriving show]
type f = {obj: o; field: string; typ: v;}
  [@@deriving show]
