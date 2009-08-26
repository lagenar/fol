let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
      while true do
	Fol.print_clauses(Fol.clause_normal_form(Parser.main Lexer.token lexbuf));
      done
  with Parsing.Parse_error ->
    print_endline "Parsing error";
    exit 1;
    | Failure(s) ->
	print_endline s;
	exit 1;
    | Lexer.Eof ->
	exit 0
