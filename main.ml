let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
      while true do
	print_endline(Fol.formula_to_str((Parser.main Lexer.token lexbuf)));
      done
  with Lexer.Eof ->
    exit 0
