let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.program Lexer.read lexbuf in
        print_int result; print_newline(); flush stdout
    done
  with Parser.Error -> failwith "Syntax error"