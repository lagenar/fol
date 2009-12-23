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
{
  open Parser
  exception Eof
}

rule token = parse
    [' ' '\t'] { token lexbuf }
  | ['\n']     { EOL }
  | ['(']      { LPAREN }
  | [')']      { RPAREN }
  | ['[']      { LBRACKET }
  | [']']      { RBRACKET }
  | [',']      { COMMA }
  | "Exists" { EXISTS }
  | "Forall" { FORALL }
  | ['A'-'Z']('_' | ['a'-'z'])* as v  { VAR_ID(v) }
  | ('_' | ['a'-'z'])+ as id { IDENTIFIER(id) }
  | ['|']      { OR }
  | ['&']      { AND }
  | ['~']      { NOT }
  | "=>"     { IMP }
  | eof        { raise Eof }
