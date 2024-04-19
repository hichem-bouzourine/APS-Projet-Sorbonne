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
  | VAR
  | PROC
  | ECHO
  | SET
  | IF
  | WHILE
  | CALL
  | VOID
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
# 34 "parser.ml"
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
  269 (* VAR *);
  270 (* PROC *);
  271 (* ECHO *);
  272 (* SET *);
  273 (* IF *);
  274 (* WHILE *);
  275 (* CALL *);
  276 (* VOID *);
  277 (* AND *);
  278 (* OR *);
  279 (* BOOL *);
  280 (* INT *);
    0|]

let yytransl_block = [|
  281 (* NUM *);
  282 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\003\000\004\000\004\000\004\000\
\004\000\004\000\004\000\005\000\005\000\005\000\005\000\006\000\
\006\000\007\000\008\000\008\000\009\000\009\000\009\000\009\000\
\009\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\011\000\011\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\004\000\007\000\008\000\
\003\000\006\000\007\000\001\000\001\000\001\000\005\000\001\000\
\003\000\003\000\001\000\003\000\002\000\003\000\004\000\003\000\
\003\000\001\000\001\000\006\000\005\000\005\000\004\000\004\000\
\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\035\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\026\000\027\000\021\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\014\000\013\000\012\000\000\000\
\000\000\000\000\009\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\022\000\000\000\024\000\000\000\
\025\000\004\000\005\000\000\000\000\000\006\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\023\000\034\000\000\000\000\000\000\000\000\000\000\000\
\000\000\018\000\020\000\032\000\000\000\000\000\000\000\031\000\
\017\000\000\000\000\000\000\000\000\000\010\000\000\000\029\000\
\030\000\015\000\000\000\007\000\011\000\028\000\008\000"

let yydgoto = "\002\000\
\004\000\005\000\015\000\016\000\060\000\061\000\047\000\048\000\
\017\000\056\000\057\000"

let yysindex = "\006\000\
\017\255\000\000\077\255\000\000\000\000\009\255\252\254\012\255\
\005\255\053\255\016\255\053\255\053\255\018\255\043\255\041\255\
\044\255\013\255\024\255\013\255\013\255\027\255\056\255\037\255\
\026\255\000\000\000\000\000\000\053\255\017\255\017\255\053\255\
\000\000\077\255\077\255\013\255\000\000\000\000\000\000\053\255\
\013\255\063\255\000\000\064\255\037\255\060\255\062\255\068\255\
\053\255\053\255\053\255\053\255\000\000\017\255\000\000\053\255\
\000\000\000\000\000\000\065\255\069\255\000\000\070\255\037\255\
\037\255\071\255\013\255\037\255\053\255\053\255\053\255\053\255\
\072\255\000\000\000\000\013\255\013\255\037\255\078\255\079\255\
\017\255\000\000\000\000\000\000\053\255\080\255\081\255\000\000\
\000\000\082\255\095\255\053\255\017\255\000\000\094\255\000\000\
\000\000\000\000\053\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\097\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\098\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\021\255\
\000\000\000\000\000\000\075\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\230\255\235\255\000\000\247\255\252\255\000\000\212\255\
\000\000\246\255\210\255"

let yytablesize = 100
let yytable = "\028\000\
\066\000\030\000\031\000\054\000\055\000\073\000\001\000\019\000\
\040\000\075\000\042\000\043\000\058\000\059\000\052\000\036\000\
\022\000\003\000\053\000\079\000\080\000\020\000\033\000\083\000\
\033\000\033\000\024\000\074\000\025\000\062\000\023\000\063\000\
\037\000\091\000\018\000\038\000\039\000\021\000\070\000\071\000\
\072\000\029\000\049\000\032\000\033\000\034\000\050\000\051\000\
\035\000\041\000\026\000\027\000\044\000\024\000\094\000\025\000\
\045\000\082\000\084\000\085\000\086\000\087\000\046\000\064\000\
\065\000\067\000\101\000\090\000\068\000\069\000\078\000\089\000\
\081\000\076\000\095\000\088\000\077\000\026\000\027\000\092\000\
\093\000\100\000\016\000\096\000\097\000\098\000\006\000\007\000\
\103\000\008\000\009\000\010\000\011\000\012\000\013\000\014\000\
\099\000\102\000\003\000\019\000"

let yycheck = "\010\000\
\045\000\012\000\013\000\030\000\031\000\052\000\001\000\012\001\
\018\000\056\000\020\000\021\000\034\000\035\000\025\000\003\001\
\012\001\001\001\029\000\064\000\065\000\026\001\002\001\068\000\
\004\001\005\001\001\001\054\000\003\001\040\000\026\001\041\000\
\020\001\078\000\026\001\023\001\024\001\026\001\049\000\050\000\
\051\000\026\001\017\001\026\001\002\001\005\001\021\001\022\001\
\005\001\026\001\025\001\026\001\026\001\001\001\081\000\003\001\
\001\001\067\000\069\000\070\000\071\000\072\000\026\001\001\001\
\001\001\006\001\093\000\077\000\007\001\002\001\001\001\076\000\
\002\001\009\001\085\000\004\001\008\001\025\001\026\001\002\001\
\002\001\092\000\008\001\004\001\004\001\004\001\010\001\011\001\
\099\000\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\002\001\004\001\002\001\002\001"

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
  VAR\000\
  PROC\000\
  ECHO\000\
  SET\000\
  IF\000\
  WHILE\000\
  CALL\000\
  VOID\000\
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
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 33 "parser.mly"
          ( _1 )
# 204 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 37 "parser.mly"
                   ( ASTBlock(_2) )
# 211 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 41 "parser.mly"
         ( ASTStat(_1) )
# 218 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 42 "parser.mly"
                    ( ASTDef(_1, _3) )
# 226 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 43 "parser.mly"
                     ( ASTStatWithCmds(_1, _3) )
# 234 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleType) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 47 "parser.mly"
                                      ( ASTConst(_2, _3, _4) )
# 243 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.singleType) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 48 "parser.mly"
                                                    ( ASTFun(_2, _3, _5, _7) )
# 253 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.singleType) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 49 "parser.mly"
                                                       ( ASTFunRec(_3, _4, _6, _8) )
# 263 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 50 "parser.mly"
                         ( ASTVar(_2, _3) )
# 271 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 51 "parser.mly"
                                     ( ASTProc(_2, _4, _6) )
# 280 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 52 "parser.mly"
                                        ( ASTProcRec(_3, _5, _7) )
# 289 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "parser.mly"
        ( Type(Int) )
# 295 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
         ( Type(Bool) )
# 301 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
         ( Type(Void) )
# 307 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.singleType list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleType) in
    Obj.repr(
# 59 "parser.mly"
                                     ( TypeFun(_2, _4) )
# 315 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 62 "parser.mly"
               ( [_1] )
# 322 "parser.ml"
               : Ast.singleType list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleType) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType list) in
    Obj.repr(
# 63 "parser.mly"
                          ( _1::_3 )
# 330 "parser.ml"
               : Ast.singleType list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 67 "parser.mly"
                         ( ASTSingleArg(_1, _3) )
# 338 "parser.ml"
               : Ast.singleArg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleArg) in
    Obj.repr(
# 70 "parser.mly"
              ( [_1] )
# 345 "parser.ml"
               : Ast.singleArg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleArg list) in
    Obj.repr(
# 71 "parser.mly"
                        ( _1::_3 )
# 353 "parser.ml"
               : Ast.singleArg list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 75 "parser.mly"
                    ( ASTEcho(_2) )
# 360 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 76 "parser.mly"
                         ( ASTSet(_2, _3) )
# 368 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 77 "parser.mly"
                              ( ASTIf(_2, _3, _4) )
# 377 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 78 "parser.mly"
                           ( ASTWhile(_2, _3) )
# 385 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr list) in
    Obj.repr(
# 79 "parser.mly"
                     ( ASTCall(_2, _3) )
# 393 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 83 "parser.mly"
        ( ASTNum(_1) )
# 400 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "parser.mly"
          ( ASTId(_1) )
# 407 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.singleExpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 85 "parser.mly"
                                                  ( ASTIf(_3, _4, _5) )
# 416 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 86 "parser.mly"
                                        ( ASTAnd(_3, _4) )
# 424 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 87 "parser.mly"
                                       ( ASTOr(_3, _4) )
# 432 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr list) in
    Obj.repr(
# 88 "parser.mly"
                               ( ASTApp(_2, _3) )
# 440 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 89 "parser.mly"
                              ( ASTLambdaExpression(_2, _4) )
# 448 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 93 "parser.mly"
                     ( [_1] )
# 455 "parser.ml"
               : Ast.singleExpr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr list) in
    Obj.repr(
# 94 "parser.mly"
                     ( _1::_2 )
# 463 "parser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.block)
