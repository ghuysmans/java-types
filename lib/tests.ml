open Types

let p_of_string s =
  match Lexer.value (Lexing.from_string s) with
  | Primitive p -> p
  | _ -> failwith "expected a primitive type"

let%test _ = p_of_string "V" = Void
let%test _ = p_of_string "Z" = Boolean
let%test _ = p_of_string "V" = Void
let%test _ = p_of_string "Z" = Boolean
let%test _ = p_of_string "B" = Byte
let%test _ = p_of_string "S" = Short
let%test _ = p_of_string "C" = Char
let%test _ = p_of_string "I" = Int
let%test _ = p_of_string "J" = Long
let%test _ = p_of_string "F" = Float
let%test _ = p_of_string "D" = Double

let parse s = Lexer.value (Lexing.from_string s)

let%test _ = parse "Ljava/lang/String;" = Object string

let%test _ = parse "[I" = Array (1, Primitive Int)
let%test _ = parse "[[I" = Array (2, Primitive Int)
let%test _ = parse "[[[I" = Array (3, Primitive Int)

let%test _ = parse "[Ljava/lang/String;" = Array (1, Object string)

let parse s = Lexer.meth (Lexing.from_string s)

let%test _ = parse "Ljava/lang/Object;->MethodName(III)Z" = {
  obj;
  meth = "MethodName";
  params = [Primitive Int; Primitive Int; Primitive Int];
  ret = Primitive Boolean;
}

let%test _ = parse "Ljava/lang/Object;->method(I[[IILjava/lang/String;[Ljava/lang/Object;)Ljava/lang/String;" = {
  obj;
  meth = "method";
  params = [
    Primitive Int;
    Array (2, Primitive Int);
    Primitive Int;
    Object string;
    Array (1, Object obj);
  ];
  ret = Object string;
}
