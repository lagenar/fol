%{
  let consts_set = ref Fol.CharSet.empty;;
  module ArityMap = Map.Make(struct type t = char let compare = compare end);;
  let arities = ref ArityMap.empty;;
%}

%token <char> PREDICATE
%token <char> EXP_ID
%token EOL
%token CONST
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
    formula EOL {arities := ArityMap.empty; $1 }
  | consts EOL formula EOL { arities := ArityMap.empty; $3 } 
;
formula:
  formula IMP formula { Fol.Connective(Fol.Imp, $1, $3) }
  | formula OR formula { Fol.Connective(Fol.Or, $1, $3) }
  | formula AND formula { Fol.Connective(Fol.And, $1, $3) }
  | NOT formula { Fol.Not($2) }
  | LPAREN formula RPAREN { $2 }
  | PREDICATE LPAREN args RPAREN  {
      let args = $3 in
      let p = Fol.Atom($1, args) in
      let ar = Fol.arity args in
	try
	  if ArityMap.find $1 !arities != ar then
	    failwith (Printf.sprintf "Wrong arity for predicate symbol %c" $1)
	  else p
	with Not_found ->
	  arities := (ArityMap.add $1 ar !arities);
	  p
    }
  | FORALL LPAREN EXP_ID RPAREN LPAREN formula RPAREN{
      if Fol.CharSet.mem $3 !consts_set then
	failwith (Printf.sprintf "Cannot quantify constant %c" $3)
      else Fol.Quantifier(Fol.Forall, $3, $6)
    }
  | EXISTS LPAREN EXP_ID RPAREN LPAREN formula RPAREN {
      if Fol.CharSet.mem $3 !consts_set then
	failwith (Printf.sprintf "Cannot quantify constant %c" $3)
      else Fol.Quantifier(Fol.Exists, $3, $6)
    }
;
args:
  expr COMMA args { Fol.Arguments($1, $3) }
  | expr { Fol.Arg($1) }
;
expr:
  EXP_ID LPAREN args RPAREN {
   let args = $3 in
   let f = Fol.FOLfunction($1, args) in
   let ar = Fol.arity args in
     try
       if ArityMap.find $1 !arities != ar then
	 failwith (Printf.sprintf "Wrong arity for function symbol %c" $1)
       else f
     with Not_found ->
       arities := (ArityMap.add $1 ar !arities);
       f
  }
  | EXP_ID { if Fol.CharSet.mem $1 !consts_set then Fol.Constant($1)
	     else Fol.Var($1)
	   }
;
consts_args:
  EXP_ID { consts_set := (Fol.CharSet.add $1 !consts_set) }
  | EXP_ID COMMA consts_args { consts_set := (Fol.CharSet.add $1 !consts_set); $3 }
consts:
  CONST LPAREN consts_args RPAREN { $3 }
;

