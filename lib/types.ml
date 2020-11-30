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

type o = {path: string list; cls: string}
(* FIXME what about internal classes? *)

type v =
  | Primitive of p
  (* reference *)
  | Object of o
  | Array of int * v

type m = {obj: o; meth: string; params: v list; ret: v;}
type f = {obj: o; field: string; typ: v;}
