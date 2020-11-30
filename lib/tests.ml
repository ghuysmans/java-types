open Types

(*
(* FIXME test them, too *)
let p_of_string = function
  | "V" -> Void
  | "Z" -> Boolean
  | "B" -> Byte
  | "S" -> Short
  | "C" -> Char
  | "I" -> Int
  | "J" -> Long
  | "F" -> Float
  | "D" -> Double
  | _ -> failwith "p_of_string"

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
*)

let o = {
  path = ["package"; "name"];
  cls = "ObjectName";
}

let parse _ = failwith "TODO"

let%test _ = parse "Lpackage/name/ObjectName;" = Object o

let string = {
  path = ["java"; "lang"];
  cls = "String";
}

let%test _ = parse "Ljava/lang/String;" = Object string

let%test _ = parse "[I" = Array (1, Primitive Int)
let%test _ = parse "[[I" = Array (2, Primitive Int)
let%test _ = parse "[[[I" = Array (3, Primitive Int)

let%test _ = parse "[Ljava/lang/String;" = Array (1, Object string)

let%test _ = parse "Lpackage/name/ObjectName;->MethodName(III)Z" = {
  obj = o;
  meth = "MethodName";
  params = [Primitive Int; Primitive Int; Primitive Int];
  ret = Primitive Boolean;
}

let%test _ = parse "Lpackage/name/ObjectName;->method(I[[IILjava/lang/String;[Ljava/lang/Object;)Ljava/lang/String;" = {
  obj = o;
  meth = "MethodName";
  params = [
    Primitive Int;
    Array (2, Primitive Int);
    Primitive Int;
    Object string;
    Array (1, Object o);
  ];
  ret = Object string;
}
