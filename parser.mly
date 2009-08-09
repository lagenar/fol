%{

%}

%token <char> PREDICATE
%token <char> EXP_ID
%token EOL
%token LPAREN RPAREN
%token COMMA
%token OR AND NOT IMP
%token EXISTS FORALL
%right IMP
%left OR
%left AND
%left EXISTS FORALL
%nonassoc NOT
%start main
%type <Fol.formula> main
%%

main:
  formula EOL { $1 }
  ;
quant_formula:
  PREDICATE LPAREN args RPAREN  { Fol.Atom($1, $3) }
  | NOT quant_formula { Fol.Not($2) }
  | LPAREN formula RPAREN { $2 }
;
formula:
  formula IMP formula { Fol.Connective(Fol.Imp, $1, $3) }
  | formula OR formula { Fol.Connective(Fol.Or, $1, $3) }
  | formula AND formula { Fol.Connective(Fol.And, $1, $3) }
  | NOT formula { Fol.Not($2) }
  | LPAREN formula RPAREN { $2 }
  | PREDICATE LPAREN args RPAREN  { Fol.Atom($1, $3) }
  | FORALL LPAREN EXP_ID RPAREN quant_formula { Fol.Quantifier(Fol.Forall, $3, $5) }
  | EXISTS LPAREN EXP_ID RPAREN quant_formula { Fol.Quantifier(Fol.Exists, $3, $5) }
;
args:
  expr COMMA args { Fol.Arguments($1, $3) }
  | expr { Fol.Arg($1) }
;
expr:
  EXP_ID LPAREN args RPAREN { Fol.FOLfunction($1, $3) }
  | EXP_ID { Fol.Var($1) }
;

