(*
 * fol
 * Copyright (C) 2009 Lucas Moauro
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
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
