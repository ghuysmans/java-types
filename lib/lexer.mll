{
  open Types

  type t =
    | Value of v
    | Right_parenthesis
    | Colon
}

(* FIXME *)
let package = ['a'-'z']+
let cls = ['A'-'Z']['A'-'Z''a'-'z']*
let meth = ['A'-'Z''a'-'z']+

rule obj path = parse
  | package as p '/' { obj (p :: path) lexbuf }
  | cls as cls ';' { Object {path = List.rev path; cls} }
and value = parse
  | 'V' { Value (Primitive Void) }
  | 'Z' { Value (Primitive Boolean) }
  | 'B' { Value (Primitive Byte) }
  | 'S' { Value (Primitive Short) }
  | 'C' { Value (Primitive Char) }
  | 'I' { Value (Primitive Int) }
  | 'J' { Value (Primitive Long) }
  | 'F' { Value (Primitive Float) }
  | 'D' { Value (Primitive Double) }
  | '['+ as dim {
    match value lexbuf with
    | Right_parenthesis -> failwith "unexpected ')'"
    | Colon -> failwith "unexpected ':'"
    | Value v -> Value (Array (String.length dim, v))
  }
  | 'L' { Value (obj [] lexbuf) }
  | ')' { Right_parenthesis }
  | ':' { Colon }

and meth' obj = parse
  | "->" meth as meth '(' {
    let rec f params =
      match value lexbuf with
      | Value v -> f (v :: params)
      | Right_parenthesis -> List.rev params
      | Colon -> failwith "unexpected ':'"
    in
    let params = f [] in
    match value lexbuf with
    | Value ret -> {obj; meth; params; ret}
    | Right_parenthesis -> failwith "unexpected ')'"
    | Colon -> failwith "unexpected ':'"
  }

{
  let value lexbuf =
    match value lexbuf with
    | Value v -> v
    | Right_parenthesis -> failwith "unexpected ')'"
    | Colon -> failwith "unexpected ':'"

  let meth lexbuf =
    match value lexbuf with
    | Object obj -> meth' obj lexbuf
    | _ -> failwith "unexpected non-object type"
}
