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
  | ADR
  | VARADR
  | NUM of (int)
  | IDENT of (string)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 36 "parser.ml"
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
  281 (* ADR *);
  282 (* VARADR *);
    0|]

let yytransl_block = [|
  283 (* NUM *);
  284 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\003\000\004\000\004\000\004\000\
\004\000\004\000\004\000\005\000\005\000\005\000\005\000\006\000\
\006\000\007\000\008\000\008\000\015\000\015\000\014\000\014\000\
\009\000\009\000\009\000\009\000\009\000\012\000\012\000\013\000\
\013\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\011\000\011\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\004\000\007\000\008\000\
\003\000\006\000\007\000\001\000\001\000\001\000\005\000\001\000\
\003\000\003\000\001\000\003\000\003\000\004\000\001\000\003\000\
\002\000\003\000\004\000\003\000\003\000\001\000\004\000\001\000\
\002\000\001\000\001\000\006\000\005\000\005\000\004\000\004\000\
\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\043\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\034\000\035\000\025\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\014\000\013\000\012\000\000\000\
\000\000\000\000\009\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\026\000\000\000\028\000\000\000\
\030\000\000\000\029\000\004\000\005\000\000\000\000\000\006\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\027\000\
\000\000\033\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\018\000\020\000\040\000\000\000\000\000\
\000\000\042\000\039\000\000\000\017\000\000\000\000\000\000\000\
\000\000\000\000\021\000\010\000\024\000\000\000\037\000\038\000\
\031\000\015\000\000\000\007\000\011\000\022\000\036\000\008\000"

let yydgoto = "\002\000\
\004\000\005\000\015\000\016\000\062\000\063\000\047\000\048\000\
\017\000\052\000\079\000\058\000\059\000\070\000\071\000"

let yysindex = "\007\000\
\020\255\000\000\085\255\000\000\000\000\251\254\006\255\254\254\
\016\255\010\255\023\255\010\255\010\255\029\255\041\255\054\255\
\056\255\012\255\035\255\012\255\012\255\043\255\071\255\045\255\
\059\255\000\000\000\000\000\000\010\255\020\255\020\255\051\255\
\000\000\085\255\085\255\012\255\000\000\000\000\000\000\010\255\
\012\255\073\255\000\000\076\255\244\254\077\255\075\255\086\255\
\010\255\010\255\010\255\010\255\000\000\020\255\000\000\028\255\
\000\000\051\255\000\000\000\000\000\000\080\255\082\255\000\000\
\090\255\045\255\244\254\065\255\091\255\104\255\100\255\012\255\
\045\255\010\255\010\255\010\255\010\255\010\255\105\255\000\000\
\083\255\000\000\012\255\012\255\045\255\106\255\108\255\107\255\
\012\255\020\255\244\254\000\000\000\000\000\000\010\255\110\255\
\111\255\000\000\000\000\112\255\000\000\113\255\116\255\010\255\
\020\255\012\255\000\000\000\000\000\000\115\255\000\000\000\000\
\000\000\000\000\010\255\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\118\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\119\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\015\255\000\000\000\000\000\000\114\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\121\255\000\000\
\000\000\000\000\000\000\000\000\000\000\120\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\235\255\012\000\000\000\242\255\029\000\000\000\195\255\
\000\000\246\255\047\000\000\000\068\000\190\255\000\000"

let yytablesize = 126
let yytable = "\028\000\
\087\000\030\000\031\000\040\000\086\000\042\000\043\000\001\000\
\054\000\055\000\024\000\093\000\025\000\068\000\036\000\069\000\
\032\000\019\000\053\000\032\000\003\000\057\000\018\000\103\000\
\109\000\021\000\065\000\022\000\024\000\064\000\025\000\037\000\
\080\000\020\000\038\000\039\000\026\000\027\000\075\000\076\000\
\077\000\078\000\033\000\023\000\049\000\060\000\061\000\057\000\
\050\000\051\000\029\000\024\000\081\000\056\000\026\000\027\000\
\032\000\092\000\034\000\024\000\035\000\025\000\041\000\094\000\
\095\000\096\000\097\000\078\000\108\000\102\000\044\000\045\000\
\046\000\066\000\107\000\049\000\067\000\026\000\027\000\050\000\
\051\000\073\000\072\000\117\000\110\000\026\000\027\000\074\000\
\083\000\084\000\085\000\118\000\088\000\116\000\006\000\007\000\
\089\000\008\000\009\000\010\000\011\000\012\000\013\000\014\000\
\120\000\090\000\091\000\104\000\099\000\105\000\100\000\101\000\
\106\000\111\000\112\000\113\000\114\000\115\000\119\000\003\000\
\019\000\016\000\023\000\041\000\098\000\082\000"

let yycheck = "\010\000\
\067\000\012\000\013\000\018\000\066\000\020\000\021\000\001\000\
\030\000\031\000\001\001\073\000\003\001\026\001\003\001\028\001\
\002\001\012\001\029\000\005\001\001\001\032\000\028\001\085\000\
\091\000\028\001\041\000\012\001\001\001\040\000\003\001\020\001\
\054\000\028\001\023\001\024\001\027\001\028\001\049\000\050\000\
\051\000\052\000\002\001\028\001\017\001\034\000\035\000\058\000\
\021\001\022\001\028\001\001\001\025\001\003\001\027\001\028\001\
\028\001\072\000\005\001\001\001\005\001\003\001\028\001\074\000\
\075\000\076\000\077\000\078\000\090\000\084\000\028\001\001\001\
\028\001\001\001\089\000\017\001\001\001\027\001\028\001\021\001\
\022\001\007\001\006\001\105\000\095\000\027\001\028\001\002\001\
\009\001\008\001\001\001\106\000\028\001\104\000\010\001\011\001\
\006\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\115\000\002\001\007\001\002\001\004\001\002\001\028\001\083\000\
\006\001\004\001\004\001\004\001\004\001\002\001\004\001\002\001\
\002\001\008\001\002\001\004\001\078\000\058\000"

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
  ADR\000\
  VARADR\000\
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
# 36 "parser.mly"
          ( _1 )
# 224 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 40 "parser.mly"
                   ( ASTBlock(_2) )
# 231 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 44 "parser.mly"
         ( ASTStat(_1) )
# 238 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 45 "parser.mly"
                    ( ASTDef(_1, _3) )
# 246 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 46 "parser.mly"
                     ( ASTStatWithCmds(_1, _3) )
# 254 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleType) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 50 "parser.mly"
                                      ( ASTConst(_2, _3, _4) )
# 263 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.singleType) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 51 "parser.mly"
                                                    ( ASTFun(_2, _3, _5, _7) )
# 273 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.singleType) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 52 "parser.mly"
                                                       ( ASTFunRec(_3, _4, _6, _8) )
# 283 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 53 "parser.mly"
                         ( ASTVar(_2, _3) )
# 291 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'argsProc) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 54 "parser.mly"
                                         ( ASTProc(_2, _4, _6) )
# 300 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'argsProc) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 55 "parser.mly"
                                            ( ASTProcRec(_3, _5, _7) )
# 309 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
        ( Type(Int) )
# 315 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
         ( Type(Bool) )
# 321 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
         ( Type(Void) )
# 327 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.singleType list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleType) in
    Obj.repr(
# 62 "parser.mly"
                                     ( TypeFun(_2, _4) )
# 335 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 65 "parser.mly"
               ( [_1] )
# 342 "parser.ml"
               : Ast.singleType list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleType) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType list) in
    Obj.repr(
# 66 "parser.mly"
                          ( _1::_3 )
# 350 "parser.ml"
               : Ast.singleType list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 70 "parser.mly"
                         ( ASTSingleArg(_1, _3) )
# 358 "parser.ml"
               : Ast.singleArg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleArg) in
    Obj.repr(
# 73 "parser.mly"
              ( [_1] )
# 365 "parser.ml"
               : Ast.singleArg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleArg list) in
    Obj.repr(
# 74 "parser.mly"
                        ( _1::_3 )
# 373 "parser.ml"
               : Ast.singleArg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 78 "parser.mly"
                         ( ASTSingleArgProc(_1, _3) )
# 381 "parser.ml"
               : 'singleArgProc))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 79 "parser.mly"
                                ( ASTSingleArgProcVar(_2, _4) )
# 389 "parser.ml"
               : 'singleArgProc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'singleArgProc) in
    Obj.repr(
# 82 "parser.mly"
                  ([_1])
# 396 "parser.ml"
               : 'argsProc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'singleArgProc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'argsProc) in
    Obj.repr(
# 83 "parser.mly"
                                (_1::_3)
# 404 "parser.ml"
               : 'argsProc))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 86 "parser.mly"
                    ( ASTEcho(_2) )
# 411 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 87 "parser.mly"
                         ( ASTSet(_2, _3) )
# 419 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 88 "parser.mly"
                              ( ASTIf(_2, _3, _4) )
# 428 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 89 "parser.mly"
                           ( ASTWhile(_2, _3) )
# 436 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprProc list) in
    Obj.repr(
# 90 "parser.mly"
                         ( ASTCall(_2, _3) )
# 444 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 94 "parser.mly"
                          (ASTExpr(_1))
# 451 "parser.ml"
               : Ast.exprProc))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 95 "parser.mly"
                          (ASTExprProcAdr(_3))
# 458 "parser.ml"
               : Ast.exprProc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprProc) in
    Obj.repr(
# 98 "parser.mly"
                      ([_1])
# 465 "parser.ml"
               : Ast.exprProc list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.exprProc) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprProc list) in
    Obj.repr(
# 99 "parser.mly"
                       ( _1::_2 )
# 473 "parser.ml"
               : Ast.exprProc list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 104 "parser.mly"
        ( ASTNum(_1) )
# 480 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 105 "parser.mly"
          ( ASTId(_1) )
# 487 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.singleExpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 106 "parser.mly"
                                                  ( ASTIf(_3, _4, _5) )
# 496 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 107 "parser.mly"
                                        ( ASTAnd(_3, _4) )
# 504 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 108 "parser.mly"
                                       ( ASTOr(_3, _4) )
# 512 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr list) in
    Obj.repr(
# 109 "parser.mly"
                               ( ASTApp(_2, _3) )
# 520 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 110 "parser.mly"
                              ( ASTLambdaExpression(_2, _4) )
# 528 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 114 "parser.mly"
                     ( [_1] )
# 535 "parser.ml"
               : Ast.singleExpr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr list) in
    Obj.repr(
# 115 "parser.mly"
                     ( _1::_2 )
# 543 "parser.ml"
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
