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
