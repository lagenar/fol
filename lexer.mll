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
  | [':']      { COLON }
  | ['A'-'Z']('_' | ['a'-'z'])* as v  { VAR_ID(v) }
  | ('_' | ['a'-'z'])+ as id { IDENTIFIER(id) }
  | ['|']      { OR }
  | ['&']      { AND }
  | ['~']      { NOT }
  | "=>"     { IMP }
  | "?" { EXISTS }
  | "!" { FORALL }
  | eof        { raise Eof }
