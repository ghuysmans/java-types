open Java

let () =
  ignore @@ Lexer.value (Lexing.from_channel stdin)
