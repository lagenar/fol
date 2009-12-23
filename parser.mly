%{
  module ArityMap = Map.Make(struct type t = string let compare = compare end);;
  let arities = ref ArityMap.empty;;

  let mk_quant q vars f =
    List.fold_right (fun x form -> Fol.Quantifier(q, x, form)) vars f
  ;;
%}

%token <string> VAR_ID
%token <string> IDENTIFIER
%token EOL
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token COMMA COLON
%token OR AND NOT IMP
%token EXISTS FORALL
%nonassoc IMP
%left OR
%left AND
%left EXISTS FORALL
%nonassoc NOT
%start main
%type <(Fol.formula)> main
%%

main:
  formula EOL { arities := ArityMap.empty; $1 }
;

formula:
  formula IMP formula { Fol.Connective(Fol.Imp, $1, $3) }
  | formula OR formula { Fol.Connective(Fol.Or, $1, $3) }
  | formula AND formula { Fol.Connective(Fol.And, $1, $3) }
  | NOT formula { Fol.Not($2) }
  | IDENTIFIER LPAREN args RPAREN { Fol.Atom($1, $3) }
  | LPAREN formula RPAREN { $2 }
  | FORALL LBRACKET variable_list RBRACKET LPAREN formula RPAREN { mk_quant (Fol.Forall) $3 $6 }
  | EXISTS LBRACKET variable_list RBRACKET LPAREN formula RPAREN { mk_quant (Fol.Exists) $3 $6 }
;

args:
  term { [$1] }
  |term COMMA args { $1::$3 }
;

term:
  VAR_ID { Fol.Var($1) }
  | IDENTIFIER { Fol.FOLfunction($1, []) }
  | IDENTIFIER LPAREN args RPAREN { Fol.FOLfunction($1, $3) }
;

variable_list:
  VAR_ID { [$1] }
  | VAR_ID COMMA variable_list { $1::$3 }
;
