{
  open Types
}

(* FIXME *)
let package = ['a'-'z']+
let cls = ['A'-'Z']['a'-'z']*
let meth = ['A'-'Z''a'-'z']+

rule obj path = parse
  | package as p '/' { obj (p :: path) lexbuf }
  | cls as cls ';' { Object {path = List.rev path; cls} }
and value = parse
  | 'V' { Primitive Void }
  | 'Z' { Primitive Boolean }
  | 'B' { Primitive Byte }
  | 'S' { Primitive Short }
  | 'C' { Primitive Char }
  | 'I' { Primitive Int }
  | 'J' { Primitive Long }
  | 'F' { Primitive Float }
  | 'D' { Primitive Double }
  | '['+ as dim { Array (String.length dim, value lexbuf) }
  | 'L' { obj [] lexbuf (* FIXME *) }
