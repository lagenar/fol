let _ =
  try
  let lexbuf = Lexing.from_channel stdin in
    while true do
      Fol.print_clauses(Fol.conjuntive_normal_form(Parser.main Lexer.token lexbuf));
    done
  with Failure(s) ->
    print_endline s
    | Parsing.Parse_error ->
	print_endline "Parsing error"
    | Lexer.Eof ->
	exit 0
