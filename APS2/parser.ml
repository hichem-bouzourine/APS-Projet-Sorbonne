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
  | NUM of (int)
  | IDENT of (string)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 41 "parser.ml"
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
    0|]

let yytransl_block = [|
  288 (* NUM *);
  289 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\003\000\004\000\004\000\004\000\
\004\000\004\000\004\000\005\000\005\000\005\000\005\000\005\000\
\006\000\006\000\007\000\008\000\008\000\015\000\015\000\014\000\
\014\000\009\000\009\000\009\000\009\000\009\000\016\000\016\000\
\012\000\012\000\013\000\013\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\011\000\
\011\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\004\000\007\000\008\000\
\003\000\006\000\007\000\001\000\001\000\001\000\004\000\005\000\
\001\000\003\000\003\000\001\000\003\000\003\000\004\000\001\000\
\003\000\002\000\003\000\004\000\003\000\003\000\001\000\005\000\
\001\000\004\000\001\000\002\000\001\000\001\000\006\000\005\000\
\005\000\004\000\004\000\004\000\004\000\005\000\006\000\001\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\050\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\037\000\038\000\026\000\000\000\031\000\000\000\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\014\000\013\000\
\012\000\000\000\000\000\000\000\009\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\027\000\000\000\029\000\000\000\033\000\
\000\000\030\000\004\000\005\000\000\000\000\000\000\000\006\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\028\000\000\000\036\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\000\021\000\043\000\000\000\000\000\000\000\044\000\
\000\000\045\000\000\000\049\000\042\000\000\000\000\000\015\000\
\018\000\000\000\000\000\000\000\000\000\000\000\022\000\010\000\
\025\000\000\000\040\000\041\000\046\000\000\000\032\000\034\000\
\016\000\000\000\007\000\011\000\023\000\039\000\047\000\008\000"

let yydgoto = "\002\000\
\004\000\005\000\015\000\016\000\070\000\071\000\049\000\050\000\
\017\000\058\000\091\000\065\000\066\000\078\000\079\000\031\000"

let yysindex = "\013\000\
\016\255\000\000\121\255\000\000\000\000\246\254\004\255\251\254\
\007\255\019\255\254\254\019\255\019\255\006\255\048\255\064\255\
\065\255\063\255\035\255\063\255\063\255\038\255\075\255\045\255\
\090\255\000\000\000\000\000\000\055\255\000\000\019\255\016\255\
\016\255\026\255\000\000\121\255\121\255\106\255\000\000\000\000\
\000\000\019\255\063\255\087\255\000\000\088\255\236\254\086\255\
\089\255\092\255\019\255\019\255\019\255\019\255\019\255\019\255\
\019\255\019\255\254\254\000\000\016\255\000\000\032\255\000\000\
\026\255\000\000\000\000\000\000\063\255\091\255\093\255\000\000\
\094\255\045\255\236\254\070\255\098\255\095\255\099\255\063\255\
\045\255\019\255\019\255\019\255\019\255\104\255\019\255\111\255\
\019\255\019\255\112\255\019\255\000\000\077\255\000\000\113\255\
\063\255\063\255\045\255\096\255\122\255\119\255\063\255\016\255\
\236\254\000\000\000\000\000\000\019\255\123\255\137\255\000\000\
\138\255\000\000\019\255\000\000\000\000\139\255\140\255\000\000\
\000\000\141\255\144\255\019\255\016\255\063\255\000\000\000\000\
\000\000\143\255\000\000\000\000\000\000\145\255\000\000\000\000\
\000\000\019\255\000\000\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\146\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\148\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\010\255\000\000\000\000\000\000\000\000\147\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\149\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\150\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\233\255\245\255\000\000\243\255\055\000\000\000\193\255\
\000\000\246\255\063\000\000\000\091\000\185\255\000\000\098\000"

let yytablesize = 157
let yytable = "\028\000\
\029\000\032\000\033\000\101\000\042\000\076\000\044\000\045\000\
\061\000\062\000\100\000\035\000\077\000\001\000\035\000\019\000\
\003\000\107\000\022\000\024\000\060\000\025\000\018\000\064\000\
\067\000\068\000\024\000\021\000\063\000\073\000\030\000\072\000\
\024\000\129\000\025\000\123\000\020\000\093\000\034\000\023\000\
\083\000\084\000\085\000\086\000\087\000\088\000\089\000\090\000\
\051\000\035\000\026\000\027\000\052\000\053\000\064\000\096\000\
\094\000\026\000\027\000\054\000\055\000\056\000\057\000\026\000\
\027\000\038\000\106\000\043\000\036\000\037\000\046\000\108\000\
\109\000\110\000\111\000\047\000\113\000\048\000\115\000\090\000\
\128\000\118\000\039\000\059\000\122\000\040\000\041\000\074\000\
\075\000\127\000\024\000\080\000\025\000\082\000\099\000\081\000\
\104\000\124\000\130\000\097\000\098\000\140\000\102\000\103\000\
\134\000\105\000\051\000\112\000\038\000\119\000\052\000\053\000\
\141\000\139\000\114\000\117\000\120\000\054\000\055\000\056\000\
\057\000\026\000\027\000\125\000\126\000\039\000\131\000\144\000\
\040\000\041\000\006\000\007\000\069\000\008\000\009\000\010\000\
\011\000\012\000\013\000\014\000\132\000\133\000\135\000\136\000\
\137\000\138\000\142\000\003\000\143\000\020\000\024\000\121\000\
\116\000\048\000\017\000\095\000\092\000"

let yycheck = "\010\000\
\003\001\012\000\013\000\075\000\018\000\026\001\020\000\021\000\
\032\000\033\000\074\000\002\001\033\001\001\000\005\001\012\001\
\001\001\081\000\012\001\001\001\031\000\003\001\033\001\034\000\
\036\000\037\000\001\001\033\001\003\001\043\000\033\001\042\000\
\001\001\105\000\003\001\099\000\033\001\061\000\033\001\033\001\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
\017\001\002\001\032\001\033\001\021\001\022\001\065\000\069\000\
\025\001\032\001\033\001\028\001\029\001\030\001\031\001\032\001\
\033\001\003\001\080\000\033\001\005\001\005\001\033\001\082\000\
\083\000\084\000\085\000\001\001\087\000\033\001\089\000\090\000\
\104\000\092\000\020\001\029\001\098\000\023\001\024\001\001\001\
\001\001\103\000\001\001\006\001\003\001\002\001\001\001\007\001\
\002\001\002\001\109\000\009\001\008\001\125\000\033\001\006\001\
\115\000\007\001\017\001\004\001\003\001\033\001\021\001\022\001\
\126\000\124\000\004\001\004\001\004\001\028\001\029\001\030\001\
\031\001\032\001\033\001\002\001\006\001\020\001\004\001\138\000\
\023\001\024\001\010\001\011\001\027\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\004\001\004\001\004\001\004\001\
\004\001\002\001\004\001\002\001\004\001\002\001\002\001\097\000\
\090\000\004\001\008\001\065\000\059\000"

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
# 37 "parser.mly"
          ( _1 )
# 258 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 41 "parser.mly"
                   ( ASTBlock(_2) )
# 265 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 45 "parser.mly"
         ( ASTStat(_1) )
# 272 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 46 "parser.mly"
                    ( ASTDef(_1, _3) )
# 280 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 47 "parser.mly"
                     ( ASTStatWithCmds(_1, _3) )
# 288 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleType) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 51 "parser.mly"
                                      ( ASTConst(_2, _3, _4) )
# 297 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.singleType) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 52 "parser.mly"
                                                    ( ASTFun(_2, _3, _5, _7) )
# 307 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.singleType) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 53 "parser.mly"
                                                       ( ASTFunRec(_3, _4, _6, _8) )
# 317 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 54 "parser.mly"
                         ( ASTVar(_2, _3) )
# 325 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'argsProc) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 55 "parser.mly"
                                         ( ASTProc(_2, _4, _6) )
# 334 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'argsProc) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 56 "parser.mly"
                                            ( ASTProcRec(_3, _5, _7) )
# 343 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
        (Type(Int))
# 349 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
         (Type(Bool))
# 355 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
         ( Type(Void) )
# 361 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleType) in
    Obj.repr(
# 63 "parser.mly"
                             (ASTVectorType(_3))
# 368 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.singleType list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleType) in
    Obj.repr(
# 64 "parser.mly"
                                     ( TypeFun(_2, _4) )
# 376 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 67 "parser.mly"
               ( [_1] )
# 383 "parser.ml"
               : Ast.singleType list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleType) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType list) in
    Obj.repr(
# 68 "parser.mly"
                          ( _1::_3 )
# 391 "parser.ml"
               : Ast.singleType list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 73 "parser.mly"
                         ( ASTSingleArg(_1, _3) )
# 399 "parser.ml"
               : Ast.singleArg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleArg) in
    Obj.repr(
# 76 "parser.mly"
              ( [_1] )
# 406 "parser.ml"
               : Ast.singleArg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleArg list) in
    Obj.repr(
# 77 "parser.mly"
                        ( _1::_3 )
# 414 "parser.ml"
               : Ast.singleArg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 81 "parser.mly"
                         ( ASTSingleArgProc(_1, _3) )
# 422 "parser.ml"
               : 'singleArgProc))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 82 "parser.mly"
                                ( ASTSingleArgProcVar(_2, _4) )
# 430 "parser.ml"
               : 'singleArgProc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'singleArgProc) in
    Obj.repr(
# 85 "parser.mly"
                  ([_1])
# 437 "parser.ml"
               : 'argsProc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'singleArgProc) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'argsProc) in
    Obj.repr(
# 86 "parser.mly"
                                (_1::_3)
# 445 "parser.ml"
               : 'argsProc))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 89 "parser.mly"
                    ( ASTEcho(_2) )
# 452 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'lValue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 90 "parser.mly"
                          ( ASTSet(_2, _3) )
# 460 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 91 "parser.mly"
                              ( ASTIf(_2, _3, _4) )
# 469 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 92 "parser.mly"
                           ( ASTWhile(_2, _3) )
# 477 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprProc list) in
    Obj.repr(
# 93 "parser.mly"
                         ( ASTCall(_2, _3) )
# 485 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "parser.mly"
          (ASTLValueIdent(_1))
# 492 "parser.ml"
               : 'lValue))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'lValue) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 98 "parser.mly"
                                    (ASTVectorValue(_3, _4))
# 500 "parser.ml"
               : 'lValue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 101 "parser.mly"
                          (ASTExpr(_1))
# 507 "parser.ml"
               : Ast.exprProc))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 102 "parser.mly"
                          (ASTExprProcAdr(_3))
# 514 "parser.ml"
               : Ast.exprProc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprProc) in
    Obj.repr(
# 105 "parser.mly"
                      ([_1])
# 521 "parser.ml"
               : Ast.exprProc list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.exprProc) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprProc list) in
    Obj.repr(
# 106 "parser.mly"
                       ( _1::_2 )
# 529 "parser.ml"
               : Ast.exprProc list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 109 "parser.mly"
        ( ASTNum(_1) )
# 536 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 110 "parser.mly"
          ( ASTId(_1) )
# 543 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.singleExpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 111 "parser.mly"
                                                  ( ASTIf(_3, _4, _5) )
# 552 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 112 "parser.mly"
                                        ( ASTAnd(_3, _4) )
# 560 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 113 "parser.mly"
                                       ( ASTOr(_3, _4) )
# 568 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr list) in
    Obj.repr(
# 114 "parser.mly"
                               ( ASTApp(_2, _3) )
# 576 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleArg list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 115 "parser.mly"
                              ( ASTLambdaExpression(_2, _4) )
# 584 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 116 "parser.mly"
                               (ASTAlloc(_3))
# 591 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 117 "parser.mly"
                             (ASTLen(_3))
# 598 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 118 "parser.mly"
                                        (ASTNth(_3, _4))
# 606 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.singleExpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleExpr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    Obj.repr(
# 119 "parser.mly"
                                                    (ASTVset(_3, _4, _5))
# 615 "parser.ml"
               : Ast.singleExpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr) in
    Obj.repr(
# 123 "parser.mly"
                     ( [_1] )
# 622 "parser.ml"
               : Ast.singleExpr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleExpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleExpr list) in
    Obj.repr(
# 124 "parser.mly"
                     ( _1::_2 )
# 630 "parser.ml"
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
