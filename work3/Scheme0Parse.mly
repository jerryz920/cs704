%{
 module P = Scheme0ParseTree
%}

%token LParen RParen Dot Eof Arrow Colon
%token <string> ID
%token <int> Num

%type <Scheme0ParseTree.program> program
%type <Scheme0ParseTree.atom> atom
%start program atom
%%
program:  atom program {($1::$2)} | Eof {[]};
expr: LParen atom_list RParen {$2};
atom_list: atom atom_list {$1::$2} | {[]};
improper: LParen atom_list Dot atom RParen {P.Improper($2,$4)};

atom:
    ID {P.Symbol $1}
  | Num {P.Numeral $1}
  | Arrow {P.Arrow}
  | Colon {P.Colon}
  | expr {P.SExpr $1}
  | improper {$1}
;
