{
  open Parser
  exception Eof
}

rule token = parse
    [' ' '\t'] { token lexbuf }
  | ['\n']     { EOL }
  | ['(']      { LPAREN }
  | [')']      { RPAREN }
  | [',']      { COMMA }
  | ['A'-'Z'] as p  { PREDICATE(p) }
  | ['v']      { OR }
  | ['a'-'z'] as e { EXP_ID(e) }
  | ['^']      { AND }
  | ['~']      { NOT }
  | "->"     { IMP }
  | "Exists" { EXISTS }
  | "ForAll" { FORALL }
  | "Const" {CONST}
  | eof        { raise Eof }
