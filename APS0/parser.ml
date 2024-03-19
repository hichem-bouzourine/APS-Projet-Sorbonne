type token =
  | LBRA
  | RBRA
  | LPAR
  | RPAR
  | SEMCOL
  | COL
  | COMA
  | ARROW
  | STAR
  | CONST
  | FUN
  | REC
  | ECHO
  | IF
  | AND
  | OR
  | BOOL
  | INT
  | NUM of (int)
  | IDENT of (string)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 28 "parser.ml"
let yytransl_const = [|
  257 (* LBRA *);
  258 (* RBRA *);
  259 (* LPAR *);
  260 (* RPAR *);
  261 (* SEMCOL *);
  262 (* COL *);
  263 (* COMA *);
  264 (* ARROW *);
  265 (* STAR *);
  266 (* CONST *);
  267 (* FUN *);
  268 (* REC *);
  269 (* ECHO *);
  270 (* IF *);
  271 (* AND *);
  272 (* OR *);
  273 (* BOOL *);
  274 (* INT *);
    0|]

let yytransl_block = [|
  275 (* NUM *);
  276 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\004\000\004\000\
\004\000\005\000\005\000\006\000\007\000\007\000\008\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\010\000\010\000\
\000\000"

let yylen = "\002\000\
\003\000\001\000\003\000\004\000\007\000\008\000\001\000\001\000\
\005\000\001\000\003\000\003\000\001\000\003\000\002\000\001\000\
\001\000\006\000\005\000\005\000\004\000\004\000\001\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\025\000\000\000\000\000\000\000\000\000\
\000\000\002\000\000\000\000\000\000\000\000\000\000\000\016\000\
\017\000\015\000\001\000\000\000\000\000\008\000\007\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\000\000\000\000\004\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\012\000\014\000\022\000\000\000\000\000\
\000\000\024\000\021\000\011\000\000\000\000\000\000\000\000\000\
\019\000\020\000\009\000\000\000\005\000\018\000\006\000"

let yydgoto = "\002\000\
\004\000\008\000\009\000\035\000\036\000\028\000\029\000\010\000\
\046\000\047\000"

let yysindex = "\005\000\
\009\255\000\000\032\255\000\000\247\254\249\254\001\255\010\255\
\014\255\000\000\023\255\002\255\023\255\011\255\013\255\000\000\
\000\000\000\000\000\000\032\255\023\255\000\000\000\000\001\255\
\023\255\033\255\038\255\039\255\045\255\001\255\001\255\001\255\
\001\255\000\000\040\255\042\255\000\000\050\255\011\255\023\255\
\011\255\001\255\001\255\001\255\001\255\001\255\048\255\023\255\
\023\255\011\255\051\255\000\000\000\000\000\000\001\255\053\255\
\054\255\000\000\000\000\000\000\055\255\052\255\001\255\056\255\
\000\000\000\000\000\000\001\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\060\255\000\000\000\000\000\000\000\000\
\000\000\000\000\047\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\059\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\044\000\000\000\246\255\017\000\000\000\224\255\000\000\
\249\255\020\000"

let yytablesize = 66
let yytable = "\018\000\
\024\000\014\000\026\000\015\000\012\000\001\000\051\000\033\000\
\053\000\003\000\011\000\019\000\013\000\014\000\038\000\015\000\
\037\000\062\000\020\000\016\000\017\000\025\000\043\000\044\000\
\045\000\021\000\030\000\031\000\032\000\052\000\027\000\016\000\
\017\000\039\000\054\000\055\000\056\000\057\000\061\000\022\000\
\023\000\005\000\006\000\040\000\007\000\041\000\042\000\064\000\
\048\000\049\000\050\000\059\000\063\000\068\000\010\000\069\000\
\065\000\066\000\067\000\070\000\071\000\013\000\023\000\034\000\
\060\000\058\000"

let yycheck = "\007\000\
\011\000\001\001\013\000\003\001\012\001\001\000\039\000\015\000\
\041\000\001\001\020\001\002\001\020\001\001\001\025\000\003\001\
\024\000\050\000\005\001\019\001\020\001\020\001\030\000\031\000\
\032\000\003\001\014\001\015\001\016\001\040\000\020\001\019\001\
\020\001\001\001\042\000\043\000\044\000\045\000\049\000\017\001\
\018\001\010\001\011\001\006\001\013\001\007\001\002\001\055\000\
\009\001\008\001\001\001\004\001\002\001\002\001\008\001\063\000\
\004\001\004\001\004\001\004\001\068\000\002\001\004\001\020\000\
\048\000\046\000"

let yynames_const = "\
  LBRA\000\
  RBRA\000\
  LPAR\000\
  RPAR\000\
  SEMCOL\000\
  COL\000\
  COMA\000\
  ARROW\000\
  STAR\000\
  CONST\000\
  FUN\000\
  REC\000\
  ECHO\000\
  IF\000\
  AND\000\
  OR\000\
  BOOL\000\
  INT\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 30 "parser.mly"
                   ( _2 )
# 164 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 34 "parser.mly"
         ( ASTStat(_1) )
# 171 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 35 "parser.mly"
                    ( ASTDef(_1, _3) )
# 179 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleType) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 39 "parser.mly"
                                      ( ASTConst(_2, _3, _4) )
# 188 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.singleType) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 40 "parser.mly"
                                                    ( ASTFun(_2, _3, _5, _7) )
# 198 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.singleType) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 41 "parser.mly"
                                                       ( ASTFunRec(_3, _4, _6, _8) )
# 208 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
        ( Type(Int) )
# 214 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
         ( Type(Bool) )
# 220 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.singleType list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleType) in
    Obj.repr(
# 47 "parser.mly"
                                     ( TypeFun(_2, _4) )
# 228 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 50 "parser.mly"
               ( [_1] )
# 235 "parser.ml"
               : Ast.singleType list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleType) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType list) in
    Obj.repr(
# 51 "parser.mly"
                          ( _1::_3 )
# 243 "parser.ml"
               : Ast.singleType list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 55 "parser.mly"
                         ( ASTSingleArg(_1, _3) )
# 251 "parser.ml"
               : Ast.singleArg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleArg) in
    Obj.repr(
# 58 "parser.mly"
              ( [_1] )
# 258 "parser.ml"
               : Ast.singleArg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleArg list) in
    Obj.repr(
# 59 "parser.mly"
                        ( _1::_3 )
# 266 "parser.ml"
               : Ast.singleArg list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 63 "parser.mly"
                    ( ASTEcho(_2) )
# 273 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 67 "parser.mly"
        ( ASTNum(_1) )
# 280 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "parser.mly"
          ( ASTId(_1) )
# 287 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.singleExpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 69 "parser.mly"
                                                  ( ASTIf(_3, _4, _5) )
# 296 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 70 "parser.mly"
                                        ( ASTAnd(_3, _4) )
# 304 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 71 "parser.mly"
                                       ( ASTOr(_3, _4) )
# 312 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr list) in
    Obj.repr(
# 72 "parser.mly"
                               ( ASTApp(_2, _3) )
# 320 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 73 "parser.mly"
                              ( ASTLambdaExpression(_2, _4) )
# 328 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 77 "parser.mly"
                     ( [_1] )
# 335 "parser.ml"
               : Ast.singleExpr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr list) in
    Obj.repr(
# 78 "parser.mly"
                     ( _1::_2 )
# 343 "parser.ml"
               : Ast.singleExpr list))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.cmds)
