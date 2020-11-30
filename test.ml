open Java

let () =
  match Sys.argv with
  | [| _ |] | [| _; "value" |] ->
    Lexer.value (Lexing.from_channel stdin) |>
    Format.printf "%a@." Types.pp_v
  | [| _; "method" |] ->
    Lexer.meth (Lexing.from_channel stdin) |>
    Format.printf "%a@." Types.pp_m
  | [| _; "field" |] ->
    Lexer.field (Lexing.from_channel stdin) |>
    Format.printf "%a@." Types.pp_f
  | _ ->
    Printf.eprintf "%s [value]|method|field\n" Sys.argv.(0);
    exit 1
