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
  | VEC
  | ALLOC
  | NTH
  | LEN
  | VSET
  | RETURN
  | NUM of (int)
  | IDENT of (string)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 42 "parser.ml"
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
  283 (* VEC *);
  284 (* ALLOC *);
  285 (* NTH *);
  286 (* LEN *);
  287 (* VSET *);
  288 (* RETURN *);
    0|]

let yytransl_block = [|
  289 (* NUM *);
  290 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\003\000\003\000\014\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\005\000\
\005\000\005\000\005\000\005\000\006\000\006\000\007\000\008\000\
\008\000\016\000\016\000\015\000\015\000\009\000\009\000\009\000\
\009\000\009\000\017\000\017\000\012\000\012\000\013\000\013\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\011\000\011\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\001\000\003\000\003\000\002\000\004\000\
\007\000\008\000\003\000\006\000\007\000\007\000\008\000\001\000\
\001\000\001\000\004\000\005\000\001\000\003\000\003\000\001\000\
\003\000\003\000\004\000\001\000\003\000\002\000\003\000\004\000\
\003\000\003\000\001\000\005\000\001\000\004\000\001\000\002\000\
\001\000\001\000\006\000\005\000\005\000\004\000\004\000\004\000\
\004\000\005\000\006\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\054\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\041\000\042\000\030\000\000\000\035\000\
\000\000\000\000\000\000\000\000\007\000\002\000\000\000\000\000\
\000\000\018\000\017\000\016\000\000\000\000\000\000\000\011\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\031\000\000\000\
\033\000\000\000\037\000\000\000\034\000\005\000\006\000\000\000\
\000\000\000\000\008\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\032\000\000\000\
\040\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\023\000\025\000\047\000\
\000\000\000\000\000\000\048\000\000\000\049\000\000\000\046\000\
\000\000\000\000\019\000\022\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\026\000\012\000\029\000\000\000\
\044\000\045\000\050\000\000\000\036\000\038\000\020\000\000\000\
\000\000\023\000\009\000\014\000\013\000\027\000\043\000\051\000\
\010\000\015\000"

let yydgoto = "\002\000\
\004\000\005\000\016\000\017\000\073\000\074\000\052\000\053\000\
\018\000\067\000\000\000\068\000\069\000\019\000\081\000\082\000\
\033\000"

let yysindex = "\012\000\
\000\255\000\000\049\255\000\000\000\000\237\254\008\255\247\254\
\058\255\018\255\023\255\018\255\018\255\250\254\018\255\037\255\
\038\255\078\255\000\000\071\255\054\255\071\255\071\255\063\255\
\098\255\066\255\110\255\000\000\000\000\000\000\072\255\000\000\
\018\255\000\255\000\255\021\255\000\000\000\000\049\255\049\255\
\013\255\000\000\000\000\000\000\018\255\071\255\101\255\000\000\
\103\255\240\254\099\255\108\255\104\255\018\255\018\255\018\255\
\018\255\018\255\018\255\018\255\021\255\023\255\000\000\000\255\
\000\000\095\255\000\000\021\255\000\000\000\000\000\000\071\255\
\112\255\102\255\000\000\129\255\045\255\240\254\111\255\127\255\
\133\255\130\255\071\255\066\255\018\255\018\255\018\255\018\255\
\132\255\018\255\138\255\018\255\142\255\018\255\000\000\023\255\
\000\000\143\255\071\255\071\255\045\255\144\255\146\255\147\255\
\149\255\148\255\071\255\000\255\240\254\000\000\000\000\000\000\
\018\255\151\255\152\255\000\000\153\255\000\000\018\255\000\000\
\154\255\155\255\000\000\000\000\156\255\150\255\159\255\071\255\
\018\255\000\255\000\255\071\255\000\000\000\000\000\000\158\255\
\000\000\000\000\000\000\160\255\000\000\000\000\000\000\018\255\
\000\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\161\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\163\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\085\255\000\000\000\000\000\000\000\000\
\145\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\164\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\233\255\033\000\000\000\242\255\068\000\000\000\186\255\
\000\000\246\255\000\000\000\000\222\255\000\000\208\255\000\000\
\198\255"

let yytablesize = 167
let yytable = "\030\000\
\003\000\034\000\035\000\094\000\037\000\045\000\103\000\047\000\
\048\000\079\000\064\000\065\000\001\000\111\000\020\000\041\000\
\061\000\080\000\026\000\021\000\027\000\026\000\063\000\066\000\
\023\000\031\000\093\000\036\000\104\000\105\000\126\000\076\000\
\042\000\097\000\075\000\043\000\044\000\122\000\038\000\072\000\
\095\000\022\000\039\000\086\000\087\000\088\000\089\000\090\000\
\091\000\092\000\028\000\029\000\127\000\028\000\029\000\061\000\
\032\000\098\000\006\000\007\000\135\000\008\000\009\000\010\000\
\011\000\012\000\013\000\014\000\110\000\024\000\079\000\070\000\
\071\000\041\000\112\000\113\000\114\000\115\000\102\000\117\000\
\015\000\119\000\040\000\121\000\134\000\125\000\039\000\046\000\
\039\000\039\000\042\000\025\000\133\000\043\000\044\000\026\000\
\049\000\027\000\050\000\051\000\062\000\077\000\136\000\078\000\
\083\000\085\000\148\000\149\000\140\000\100\000\026\000\054\000\
\027\000\146\000\084\000\055\000\056\000\150\000\147\000\096\000\
\099\000\154\000\057\000\058\000\059\000\060\000\054\000\028\000\
\029\000\101\000\055\000\056\000\107\000\153\000\108\000\116\000\
\109\000\057\000\058\000\059\000\060\000\118\000\028\000\029\000\
\106\000\120\000\123\000\129\000\130\000\128\000\131\000\144\000\
\021\000\132\000\137\000\138\000\139\000\141\000\142\000\143\000\
\145\000\151\000\003\000\152\000\024\000\028\000\124\000"

let yycheck = "\010\000\
\001\001\012\000\013\000\062\000\015\000\020\000\077\000\022\000\
\023\000\026\001\034\000\035\000\001\000\084\000\034\001\003\001\
\027\000\034\001\001\001\012\001\003\001\001\001\033\000\003\001\
\034\001\003\001\061\000\034\001\077\000\078\000\101\000\046\000\
\020\001\068\000\045\000\023\001\024\001\096\000\002\001\027\001\
\064\000\034\001\005\001\054\000\055\000\056\000\057\000\058\000\
\059\000\060\000\033\001\034\001\101\000\033\001\034\001\066\000\
\034\001\072\000\010\001\011\001\109\000\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\083\000\012\001\026\001\039\000\
\040\000\003\001\085\000\086\000\087\000\088\000\034\001\090\000\
\032\001\092\000\005\001\094\000\108\000\100\000\002\001\034\001\
\004\001\005\001\020\001\034\001\107\000\023\001\024\001\001\001\
\034\001\003\001\001\001\034\001\029\001\001\001\113\000\001\001\
\006\001\002\001\130\000\131\000\119\000\008\001\001\001\017\001\
\003\001\128\000\007\001\021\001\022\001\132\000\129\000\025\001\
\009\001\145\000\028\001\029\001\030\001\031\001\017\001\033\001\
\034\001\001\001\021\001\022\001\006\001\144\000\002\001\004\001\
\007\001\028\001\029\001\030\001\031\001\004\001\033\001\034\001\
\034\001\004\001\004\001\002\001\002\001\006\001\002\001\002\001\
\008\001\006\001\004\001\004\001\004\001\004\001\004\001\004\001\
\002\001\004\001\002\001\004\001\002\001\002\001\099\000"

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
  VEC\000\
  ALLOC\000\
  NTH\000\
  LEN\000\
  VSET\000\
  RETURN\000\
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
# 39 "parser.mly"
          ( _1 )
# 271 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 43 "parser.mly"
                   ( ASTBlock(_2) )
# 278 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 47 "parser.mly"
         ( ASTStat(_1) )
# 285 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.ret) in
    Obj.repr(
# 48 "parser.mly"
        ( ASTRet(_1))
# 292 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 49 "parser.mly"
                    ( ASTDef(_1, _3) )
# 300 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 50 "parser.mly"
                     ( ASTStatWithCmds(_1, _3) )
# 308 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 54 "parser.mly"
                      ( ASTReturn(_2) )
# 315 "parser.ml"
               : Ast.ret))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleType) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 57 "parser.mly"
                                      ( ASTConst(_2, _3, _4) )
# 324 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.singleType) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 58 "parser.mly"
                                                    ( ASTFun(_2, _3, _5, _7) )
# 334 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.singleType) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 59 "parser.mly"
                                                       ( ASTFunRec(_3, _4, _6, _8) )
# 344 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 60 "parser.mly"
                         ( ASTVar(_2, _3) )
# 352 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'argsProc) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 61 "parser.mly"
                                         ( ASTProc(_2, _4, _6) )
# 361 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'argsProc) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 62 "parser.mly"
                                            ( ASTProcRec(_3, _5, _7) )
# 370 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.singleType) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'argsProc) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 63 "parser.mly"
                                                  ( ASTFunBlock(_2, _3, _5, _7) )
# 380 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.singleType) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'argsProc) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 64 "parser.mly"
                                                      ( ASTFunRecBlock(_3, _4, _6, _8) )
# 390 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
        (Type(Int))
# 396 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
         (Type(Bool))
# 402 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
         ( Type(Void) )
# 408 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleType) in
    Obj.repr(
# 71 "parser.mly"
                             (ASTVectorType(_3))
# 415 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.singleType list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleType) in
    Obj.repr(
# 72 "parser.mly"
                                     ( TypeFun(_2, _4) )
# 423 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 75 "parser.mly"
               ( [_1] )
# 430 "parser.ml"
               : Ast.singleType list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleType) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType list) in
    Obj.repr(
# 76 "parser.mly"
                          ( _1::_3 )
# 438 "parser.ml"
               : Ast.singleType list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 81 "parser.mly"
                         ( ASTSingleArg(_1, _3) )
# 446 "parser.ml"
               : Ast.singleArg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleArg) in
    Obj.repr(
# 84 "parser.mly"
              ( [_1] )
# 453 "parser.ml"
               : Ast.singleArg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleArg list) in
    Obj.repr(
# 85 "parser.mly"
                        ( _1::_3 )
# 461 "parser.ml"
               : Ast.singleArg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 89 "parser.mly"
                         ( ASTSingleArgProc(_1, _3) )
# 469 "parser.ml"
               : 'singleArgProc))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 90 "parser.mly"
                                ( ASTSingleArgProcVar(_2, _4) )
# 477 "parser.ml"
               : 'singleArgProc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'singleArgProc) in
    Obj.repr(
# 93 "parser.mly"
                  ([_1])
# 484 "parser.ml"
               : 'argsProc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'singleArgProc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'argsProc) in
    Obj.repr(
# 94 "parser.mly"
                                (_1::_3)
# 492 "parser.ml"
               : 'argsProc))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 97 "parser.mly"
                    ( ASTEcho(_2) )
# 499 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'lValue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 98 "parser.mly"
                          ( ASTSet(_2, _3) )
# 507 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 99 "parser.mly"
                              ( ASTIf(_2, _3, _4) )
# 516 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 100 "parser.mly"
                           ( ASTWhile(_2, _3) )
# 524 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprProc list) in
    Obj.repr(
# 101 "parser.mly"
                         ( ASTCall(_2, _3) )
# 532 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 105 "parser.mly"
          (ASTLValueIdent(_1))
# 539 "parser.ml"
               : 'lValue))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'lValue) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 106 "parser.mly"
                                    (ASTVectorValue(_3, _4))
# 547 "parser.ml"
               : 'lValue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 109 "parser.mly"
                          (ASTExpr(_1))
# 554 "parser.ml"
               : Ast.exprProc))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'lValue) in
    Obj.repr(
# 110 "parser.mly"
                           (ASTExprProcAdr(_3))
# 561 "parser.ml"
               : Ast.exprProc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprProc) in
    Obj.repr(
# 113 "parser.mly"
                      ([_1])
# 568 "parser.ml"
               : Ast.exprProc list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.exprProc) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprProc list) in
    Obj.repr(
# 114 "parser.mly"
                       ( _1::_2 )
# 576 "parser.ml"
               : Ast.exprProc list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 117 "parser.mly"
        ( ASTNum(_1) )
# 583 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 118 "parser.mly"
          ( ASTId(_1) )
# 590 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.singleExpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 119 "parser.mly"
                                                  ( ASTIf(_3, _4, _5) )
# 599 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 120 "parser.mly"
                                        ( ASTAnd(_3, _4) )
# 607 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 121 "parser.mly"
                                       ( ASTOr(_3, _4) )
# 615 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.exprProc list) in
    Obj.repr(
# 122 "parser.mly"
                                   ( ASTApp(_2, _3) )
# 623 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 123 "parser.mly"
                              ( ASTLambdaExpression(_2, _4) )
# 631 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 124 "parser.mly"
                               (ASTAlloc(_3))
# 638 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 125 "parser.mly"
                             (ASTLen(_3))
# 645 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 126 "parser.mly"
                                        (ASTNth(_3, _4))
# 653 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.singleExpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 127 "parser.mly"
                                                    (ASTVset(_3, _4, _5))
# 662 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 131 "parser.mly"
                     ( [_1] )
# 669 "parser.ml"
               : Ast.singleExpr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr list) in
    Obj.repr(
# 132 "parser.mly"
                     ( _1::_2 )
# 677 "parser.ml"
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
