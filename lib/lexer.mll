{
  open Types

  type t =
    | Value of v
    | Right_parenthesis
    | Colon

  let expect_value = function
    | Value v -> v
    | Right_parenthesis -> failwith "unexpected ')'"
    | Colon -> failwith "unexpected ':'"
}

(* FIXME *)
let package = ['a'-'z']+
let cls = ['A'-'Z']['A'-'Z''a'-'z']*
let field_or_meth = ['A'-'Z''a'-'z']+

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
  | '['+ as dim { Value (Array (String.length dim, expect_value (value lexbuf))) }
  | 'L' { Value (obj [] lexbuf) }
  | ')' { Right_parenthesis }
  | ':' { Colon }

and meth' obj = parse
  | "->" (field_or_meth as meth) '(' {
    let rec f params =
      match value lexbuf with
      | Value v -> f (v :: params)
      | Right_parenthesis -> List.rev params
      | Colon -> failwith "unexpected ':'"
    in
    let params = f [] in
    {obj; meth; params; ret = expect_value (value lexbuf)}
  }

and field' obj = parse
  | "->" (field_or_meth as field) ':' {
    {obj; field; typ = expect_value (value lexbuf)}
  }

{
  let value lexbuf =
    expect_value (value lexbuf)

  let access k lexbuf =
    match value lexbuf with
    | Object obj -> k obj lexbuf
    | _ -> failwith "unexpected non-object type"

  let meth = access meth'
  let field = access field'
}
