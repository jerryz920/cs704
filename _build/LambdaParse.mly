%{
  open Lambda
%}

%token LParen RParen LSquare RSquare LBrace RBrace
%token Dot Arrow Tick Equals Eof
%token Lambda If Then Else Let Letrec In Quote True False Nil Int Bool Sym
%token <string> ID
%token <int> Num

%type <Lambda.expr> expr
%type <Lambda.value> value
%type <Lambda.typ> typ
%type <Lambda.gentyp> gentyp
%start expr value typ gentyp
%%

expr:
  value                        {Val $1}
| ID                           {Var $1}
| LParen expr_in_parens RParen {$2}
;
  
expr_in_parens: 
  If expr Then expr Else expr           {If ($2, $4, $6)}
| Lambda LParen ID RParen expr          {Lambda ($3, $5)}
| expr expr                             {Apply ($1, $2) }
| Let LParen def RParen In expr         {Let ($3, $6)}
| Letrec LParen def RParen In expr      {Letrec ($3, $6)}
;

def: ID Equals expr  {$1, $3};

value:
  Num    {Number $1}
| True   {Boolean true}
| False  {Boolean false}
| Nil    {Nil}
| LParen Quote quotation RParen {$3}
| Tick quotation {$2}
;  

quotation_list:
  {Nil}
| quotation quotation_list {Pair($1, $2)}
| quotation Dot quotation {Pair($1, $3)}

quotation:
  ID  {Symbol $1}
| Lambda {Symbol "lambda"}
| If {Symbol "if"}
| Then {Symbol "then"}
| Else {Symbol "else"}
| Let {Symbol "let"}
| Letrec {Symbol "letrec"}
| In {Symbol "in"}
| Quote {Symbol "quote"}
| Int {Symbol "int"}
| Bool {Symbol "bool"}
| Sym {Symbol "sym"}
| True {Boolean true}
| False {Boolean false}
| Nil {Nil}
| Num {Number $1}
| LParen quotation_list RParen {$2}
;

gentyp: LBrace generic_list RBrace typ {$2, $4} | typ {[], $1}
generic_list: {[]} | Tick ID generic_list {$2::$3};
typ:
  LParen typ Arrow typ RParen {Fun($2,$4)}
| LSquare typ RSquare {List $2}
| Tick ID {Tvar $2}
| Int {Int}
| Bool {Bool}
| Sym {Sym}

