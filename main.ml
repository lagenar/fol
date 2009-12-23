let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
      while true do
	Cnl.print_clauses(Cnl.clause_normal_form(Parser.main Lexer.token lexbuf));
      done
  with Parsing.Parse_error ->
    print_endline "Parse error";
    exit 1;
    | Error.Arity_Error(id) ->
	print_endline id;
	exit 2;
    | Error.Type_Error(id) ->
	print_endline id;
	exit 3;
    | Lexer.Eof ->
	exit 0
