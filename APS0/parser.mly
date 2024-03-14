(*Grammaire: lexemes -> AST*)
%{
open Ast
%}

(*lexemes*)
(*Symboles reserves*)
%token LBRA RBRA
%token LPAR RPAR
%token SEMCOL COL COMA ARROW STAR
(*Mots clef*)
%token CONST FUN REC ECHO IF
%token AND OR
%token BOOL INT
(*Constantes numeriques*)
%token <int> NUM
(*Identificateurs*)
%token <string> IDENT

(*Types grammaire*)
(*Programme*)
%type <Ast.cmds> prog
(*Suite de commandes*)
%type <Ast.cmds> cmds
(*Definition*)
%type <Ast.def> def
(*Type*)
%type <Ast.singleType> singleType
%type <Ast.singleType list> types
(*Parametres formels*)
%type <Ast.singleArg> singleArg
%type <Ast.singleArg list> args
(*Instruction*)
%type <Ast.stat> stat
(*Expression*)
%type <Ast.singleExpr> singleExpr
(*Suite d’expressions*)
%type <Ast.singleExpr list> exprs

%start prog

%%
(*Programme*)
prog: 
  | LBRA cmds RBRA { ASTCmds($2) }
;

(*Suite de commandes*)
cmds:
  | stat { ASTStat($1) }
  | def SEMCOL cmds { ASTDef($1), ASTCmds($3) }
;

(*Definition*)
def:
  | CONST IDENT singleType singleExpr { ASTConst($2, $3, $4) }
  | FUN IDENT singleType LBRA args RBRA singleExpr  { ASTFun($2, $3, $5, $7) }
  | FUN REC IDENT singleType LBRA args RBRA singleExpr { ASTFunRec($3, $4, $6, $8) } 
;

(*Type*)
singleType:
  | INT { Type(Int) }
  | BOOL { Type(Bool) }  
  | LPAR types ARROW singleType RPAR { TypeFun($2, $4) }
;
types:
  | singleType { [$1] }
  | singleType STAR types { $1::$3 }
;

(*Parametres formels*)
singleArg: 
  | IDENT COL singleType { ASTSingleArg($1, $3) }
;
args:
  | singleArg { [$1] }
  | singleArg COMA args { $1::$3 }
;

(*Instruction*)
stat:
  | ECHO singleExpr { ASTEcho($2) }
;

(*Expression*)
singleExpr:
  | NUM { ASTNum($1) }
  | IDENT { ASTId($1) }
  | LPAR IF singleExpr singleExpr singleExpr RPAR { ASTIf($3, $4, $5) }
  | LPAR AND singleExpr singleExpr RPAR { ASTAnd($3, $4) }
  | LPAR OR singleExpr singleExpr RPAR { ASTOr($3, $4) }
  | LPAR singleExpr exprs RPAR { ASTApp($2, $3) }
  | LBRA args RBRA singleExpr { ASTLambdaExpression($2, $4) }
;

(*Suite d’expressions*)
exprs :
  | singleExpr       { [$1] }
  | singleExpr exprs { $1::$2 }
;