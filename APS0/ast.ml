type cmds =
  | ASTStat of stat
  | ASTDef of def * cmds

type def =
  | ASTConst of string * singleType * singleExpr
  | ASTFun of string * singleType * args * singleExpr
  | ASTFunRec of string * singleType * args * singleExpr

type singleType = 
  | Int
  | Bool
  | TypeFun of types * singleType
  and types = 
  | singleType list

type singleArg = 
  | ASTSingleArg of string * singleType
  and args = 
  | singleArg list (* <-- inspiré d'un étudiant dans la salle TME*)

type stat =
  | ASTEcho of singleExpr

type singleExpr =
  | ASTNum of int
  | ASTId of string
  | ASTIf of singleExpr * singleExpr * singleExpr
  | ASTAnd of singleExpr * singleExpr
  | ASTOr of singleExpr * singleExpr
  | ASTApp of singleExpr * singleExpr list
  | ASTLambdaExpression of args * singleExpr
  and exprs =
  | singleExpr list