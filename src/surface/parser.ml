type token =
  | IMPORT of (Support.Error.info)
  | AS of (Support.Error.info)
  | USTRING of (Support.Error.info)
  | IF of (Support.Error.info)
  | THEN of (Support.Error.info)
  | ELSE of (Support.Error.info)
  | TRUE of (Support.Error.info)
  | FALSE of (Support.Error.info)
  | BOOL of (Support.Error.info)
  | CASE of (Support.Error.info)
  | OF of (Support.Error.info)
  | UNIT of (Support.Error.info)
  | UUNIT of (Support.Error.info)
  | TIMESFLOAT of (Support.Error.info)
  | UFLOAT of (Support.Error.info)
  | LET of (Support.Error.info)
  | IN of (Support.Error.info)
  | INERT of (Support.Error.info)
  | LAMBDA of (Support.Error.info)
  | FIX of (Support.Error.info)
  | LETREC of (Support.Error.info)
  | TYPE of (Support.Error.info)
  | ISZERO of (Support.Error.info)
  | INT of (Support.Error.info)
  | INDUCTIVE of (Support.Error.info)
  | MATCH of (Support.Error.info)
  | WITH of (Support.Error.info)
  | DASH of (Support.Error.info)
  | END of (Support.Error.info)
  | CONFIG of (Support.Error.info)
  | UCID of (string Support.Error.withinfo)
  | LCID of (string Support.Error.withinfo)
  | INTV of (int Support.Error.withinfo)
  | FLOATV of (float Support.Error.withinfo)
  | STRINGV of (string Support.Error.withinfo)
  | BINARYINTOP of (string Support.Error.withinfo)
  | APOSTROPHE of (Support.Error.info)
  | DQUOTE of (Support.Error.info)
  | ARROW of (Support.Error.info)
  | BANG of (Support.Error.info)
  | BARGT of (Support.Error.info)
  | BARRCURLY of (Support.Error.info)
  | BARRSQUARE of (Support.Error.info)
  | COLON of (Support.Error.info)
  | COLONCOLON of (Support.Error.info)
  | COLONEQ of (Support.Error.info)
  | COLONHASH of (Support.Error.info)
  | COMMA of (Support.Error.info)
  | DARROW of (Support.Error.info)
  | DDARROW of (Support.Error.info)
  | DOT of (Support.Error.info)
  | EOF of (Support.Error.info)
  | EQ of (Support.Error.info)
  | EQEQ of (Support.Error.info)
  | EXISTS of (Support.Error.info)
  | GT of (Support.Error.info)
  | LEQ of (Support.Error.info)
  | GEQ of (Support.Error.info)
  | HASH of (Support.Error.info)
  | LCURLY of (Support.Error.info)
  | LCURLYBAR of (Support.Error.info)
  | LEFTARROW of (Support.Error.info)
  | LPAREN of (Support.Error.info)
  | LSQUARE of (Support.Error.info)
  | LSQUAREBAR of (Support.Error.info)
  | LT of (Support.Error.info)
  | RCURLY of (Support.Error.info)
  | RPAREN of (Support.Error.info)
  | RSQUARE of (Support.Error.info)
  | SEMI of (Support.Error.info)
  | PLUS of (Support.Error.info)
  | SLASH of (Support.Error.info)
  | STAR of (Support.Error.info)
  | TRIANGLE of (Support.Error.info)
  | USCORE of (Support.Error.info)
  | VBAR of (Support.Error.info)
  | ALIGN of (Support.Error.info)
  | LABEL of (Support.Error.info)
  | UNLABEL of (Support.Error.info)
  | OR of (Support.Error.info)
  | AND of (Support.Error.info)
  | NOT of (Support.Error.info)
  | RDASH of (Support.Error.info)
  | COMPRESS of (Support.Error.info)
  | DECRO of (Support.Error.info)

open Parsing;;
let _ = parse_error;;
# 7 "parser.mly"
open Support.Error
open Support.Pervasive
open Syntax
# 95 "parser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* IMPORT *);
  258 (* AS *);
  259 (* USTRING *);
  260 (* IF *);
  261 (* THEN *);
  262 (* ELSE *);
  263 (* TRUE *);
  264 (* FALSE *);
  265 (* BOOL *);
  266 (* CASE *);
  267 (* OF *);
  268 (* UNIT *);
  269 (* UUNIT *);
  270 (* TIMESFLOAT *);
  271 (* UFLOAT *);
  272 (* LET *);
  273 (* IN *);
  274 (* INERT *);
  275 (* LAMBDA *);
  276 (* FIX *);
  277 (* LETREC *);
  278 (* TYPE *);
  279 (* ISZERO *);
  280 (* INT *);
  281 (* INDUCTIVE *);
  282 (* MATCH *);
  283 (* WITH *);
  284 (* DASH *);
  285 (* END *);
  286 (* CONFIG *);
  287 (* UCID *);
  288 (* LCID *);
  289 (* INTV *);
  290 (* FLOATV *);
  291 (* STRINGV *);
  292 (* BINARYINTOP *);
  293 (* APOSTROPHE *);
  294 (* DQUOTE *);
  295 (* ARROW *);
  296 (* BANG *);
  297 (* BARGT *);
  298 (* BARRCURLY *);
  299 (* BARRSQUARE *);
  300 (* COLON *);
  301 (* COLONCOLON *);
  302 (* COLONEQ *);
  303 (* COLONHASH *);
  304 (* COMMA *);
  305 (* DARROW *);
  306 (* DDARROW *);
  307 (* DOT *);
    0 (* EOF *);
  308 (* EQ *);
  309 (* EQEQ *);
  310 (* EXISTS *);
  311 (* GT *);
  312 (* LEQ *);
  313 (* GEQ *);
  314 (* HASH *);
  315 (* LCURLY *);
  316 (* LCURLYBAR *);
  317 (* LEFTARROW *);
  318 (* LPAREN *);
  319 (* LSQUARE *);
  320 (* LSQUAREBAR *);
  321 (* LT *);
  322 (* RCURLY *);
  323 (* RPAREN *);
  324 (* RSQUARE *);
  325 (* SEMI *);
  326 (* PLUS *);
  327 (* SLASH *);
  328 (* STAR *);
  329 (* TRIANGLE *);
  330 (* USCORE *);
  331 (* VBAR *);
  332 (* ALIGN *);
  333 (* LABEL *);
  334 (* UNLABEL *);
  335 (* OR *);
  336 (* AND *);
  337 (* NOT *);
  338 (* RDASH *);
  339 (* COMPRESS *);
  340 (* DECRO *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\004\000\004\000\004\000\009\000\
\009\000\008\000\008\000\006\000\006\000\010\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\013\000\013\000\014\000\
\014\000\011\000\011\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\017\000\017\000\018\000\018\000\
\018\000\018\000\018\000\019\000\019\000\016\000\016\000\016\000\
\016\000\016\000\016\000\016\000\016\000\021\000\020\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\022\000\022\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\023\000\023\000\007\000\007\000\000\000"

let yylen = "\002\000\
\001\000\004\000\000\000\003\000\002\000\004\000\001\000\002\000\
\002\000\004\000\008\000\001\000\001\000\001\000\001\000\003\000\
\005\000\002\000\004\000\002\000\002\000\001\000\003\000\001\000\
\001\000\001\000\003\000\001\000\002\000\001\000\003\000\003\000\
\001\000\003\000\001\000\001\000\006\000\006\000\006\000\006\000\
\006\000\006\000\008\000\005\000\003\000\005\000\001\000\001\000\
\003\000\002\000\003\000\001\000\003\000\001\000\002\000\002\000\
\002\000\002\000\002\000\003\000\002\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\003\000\001\000\001\000\001\000\001\000\
\003\000\001\000\001\000\003\000\000\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\001\000\000\000\087\000\000\000\000\000\000\000\
\000\000\077\000\078\000\079\000\000\000\000\000\000\000\000\000\
\000\000\000\000\064\000\000\000\000\000\000\000\082\000\067\000\
\071\000\069\000\070\000\000\000\000\000\068\000\063\000\066\000\
\065\000\000\000\000\000\000\000\073\000\072\000\062\000\000\000\
\000\000\007\000\000\000\033\000\000\000\000\000\000\000\004\000\
\005\000\080\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\009\000\000\000\000\000\
\008\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\
\025\000\028\000\026\000\000\000\000\000\000\000\086\000\022\000\
\000\000\020\000\021\000\000\000\081\000\000\000\076\000\000\000\
\002\000\032\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\047\000\000\000\
\000\000\012\000\013\000\015\000\014\000\006\000\000\000\000\000\
\000\000\029\000\000\000\084\000\075\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\050\000\000\000\
\000\000\000\000\044\000\000\000\000\000\027\000\023\000\034\000\
\000\000\037\000\038\000\039\000\040\000\041\000\000\000\000\000\
\000\000\000\000\049\000\051\000\000\000\031\000\042\000\000\000\
\019\000\000\000\053\000\000\000\043\000\011\000\046\000"

let yydgoto = "\002\000\
\005\000\006\000\041\000\126\000\066\000\065\000\062\000\115\000\
\000\000\127\000\096\000\097\000\128\000\043\000\044\000\045\000\
\120\000\121\000\145\000\046\000\047\000\069\000\067\000"

let yysindex = "\008\000\
\001\000\000\000\000\000\240\254\000\000\006\255\189\254\254\254\
\172\255\000\000\000\000\000\000\226\254\248\254\048\255\003\255\
\009\255\172\255\000\000\017\255\247\254\220\254\000\000\000\000\
\000\000\000\000\000\000\172\255\172\255\000\000\000\000\000\000\
\000\000\048\255\048\255\048\255\000\000\000\000\000\000\020\255\
\236\254\000\000\016\255\000\000\048\255\048\255\048\255\000\000\
\000\000\000\000\049\255\027\255\037\255\025\255\054\255\016\255\
\058\255\051\255\077\255\063\255\033\255\000\000\033\255\172\255\
\000\000\072\255\040\255\053\255\060\255\016\255\016\255\016\255\
\084\255\001\000\104\255\016\255\214\255\016\255\172\255\172\255\
\172\255\033\255\033\255\033\255\108\255\079\255\012\255\000\000\
\000\000\000\000\000\000\033\255\033\255\033\255\000\000\000\000\
\100\255\000\000\000\000\172\255\000\000\172\255\000\000\033\255\
\000\000\000\000\016\255\137\255\127\255\128\255\103\255\105\255\
\099\255\033\255\130\255\079\255\079\255\079\255\000\000\131\255\
\120\255\000\000\000\000\000\000\000\000\000\000\113\255\101\255\
\110\255\000\000\033\255\000\000\000\000\115\255\172\255\172\255\
\172\255\172\255\172\255\172\255\106\255\147\255\000\000\134\255\
\102\255\116\255\000\000\172\255\033\255\000\000\000\000\000\000\
\172\255\000\000\000\000\000\000\000\000\000\000\168\255\108\255\
\135\255\079\255\000\000\000\000\111\255\000\000\000\000\172\255\
\000\000\108\255\000\000\079\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\093\255\000\000\000\000\000\000\000\000\000\000\093\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\121\255\228\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\251\255\000\000\024\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\039\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\123\255\000\000\129\255\000\000\083\000\127\000\171\000\
\000\000\093\255\000\000\215\000\000\000\003\001\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\051\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\047\001\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\125\255\069\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\133\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\255\000\000\000\000\136\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\166\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\123\000\194\000\000\000\000\000\250\255\000\000\000\000\107\255\
\000\000\211\000\072\000\112\000\058\000\051\002\000\000\000\000\
\036\000\144\255\047\000\000\000\000\000\108\000\111\000"

let yytablesize = 640
let yytable = "\042\000\
\003\000\052\000\051\000\143\000\144\000\146\000\008\000\063\000\
\001\000\009\000\169\000\059\000\010\000\011\000\007\000\064\000\
\004\000\012\000\122\000\123\000\174\000\013\000\068\000\054\000\
\014\000\015\000\016\000\018\000\036\000\036\000\017\000\018\000\
\049\000\019\000\057\000\020\000\021\000\022\000\023\000\058\000\
\036\000\088\000\061\000\053\000\124\000\089\000\125\000\060\000\
\074\000\144\000\036\000\073\000\036\000\079\000\010\000\011\000\
\090\000\099\000\024\000\012\000\025\000\026\000\027\000\091\000\
\028\000\055\000\075\000\029\000\082\000\018\000\030\000\036\000\
\108\000\109\000\110\000\031\000\032\000\033\000\080\000\050\000\
\023\000\034\000\035\000\036\000\037\000\038\000\039\000\040\000\
\081\000\036\000\036\000\092\000\036\000\003\000\093\000\068\000\
\003\000\083\000\036\000\003\000\003\000\084\000\085\000\086\000\
\003\000\101\000\028\000\048\000\003\000\029\000\116\000\003\000\
\003\000\003\000\087\000\094\000\048\000\003\000\003\000\100\000\
\003\000\102\000\003\000\003\000\003\000\003\000\103\000\104\000\
\154\000\155\000\156\000\157\000\158\000\159\000\048\000\048\000\
\106\000\117\000\131\000\114\000\118\000\165\000\135\000\136\000\
\137\000\003\000\167\000\003\000\003\000\003\000\140\000\003\000\
\119\000\138\000\003\000\139\000\142\000\003\000\148\000\147\000\
\149\000\173\000\003\000\003\000\003\000\153\000\150\000\163\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\009\000\
\151\000\161\000\010\000\011\000\160\000\162\000\164\000\012\000\
\168\000\172\000\170\000\013\000\083\000\085\000\014\000\015\000\
\016\000\010\000\045\000\074\000\105\000\018\000\030\000\019\000\
\048\000\052\000\152\000\050\000\023\000\130\000\166\000\175\000\
\171\000\133\000\132\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\010\000\011\000\000\000\000\000\
\024\000\012\000\025\000\026\000\027\000\000\000\028\000\000\000\
\000\000\029\000\080\000\080\000\030\000\000\000\000\000\080\000\
\000\000\031\000\032\000\033\000\000\000\050\000\023\000\034\000\
\035\000\036\000\037\000\038\000\039\000\040\000\000\000\054\000\
\054\000\054\000\054\000\080\000\080\000\000\000\054\000\000\000\
\075\000\000\000\000\000\054\000\000\000\000\000\000\000\095\000\
\028\000\098\000\000\000\029\000\000\000\054\000\080\000\054\000\
\000\000\000\000\054\000\054\000\000\000\000\000\080\000\000\000\
\000\000\080\000\000\000\000\000\111\000\112\000\113\000\000\000\
\080\000\000\000\054\000\059\000\059\000\059\000\059\000\129\000\
\000\000\000\000\059\000\000\000\000\000\054\000\000\000\059\000\
\054\000\000\000\134\000\000\000\054\000\054\000\000\000\054\000\
\000\000\059\000\000\000\059\000\141\000\054\000\059\000\059\000\
\000\000\000\000\000\000\000\000\000\000\035\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\000\000\059\000\057\000\
\057\000\057\000\057\000\000\000\000\000\000\000\057\000\000\000\
\000\000\059\000\035\000\057\000\059\000\035\000\035\000\000\000\
\059\000\059\000\000\000\059\000\000\000\057\000\000\000\057\000\
\000\000\059\000\057\000\057\000\035\000\035\000\000\000\035\000\
\000\000\000\000\000\000\000\000\000\000\035\000\000\000\000\000\
\000\000\000\000\057\000\055\000\055\000\055\000\055\000\000\000\
\000\000\000\000\055\000\000\000\000\000\057\000\000\000\055\000\
\057\000\000\000\000\000\000\000\057\000\057\000\000\000\057\000\
\000\000\055\000\000\000\055\000\000\000\057\000\055\000\055\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\055\000\056\000\
\056\000\056\000\056\000\000\000\000\000\000\000\056\000\000\000\
\000\000\055\000\000\000\056\000\055\000\000\000\000\000\000\000\
\055\000\055\000\000\000\055\000\000\000\056\000\000\000\056\000\
\000\000\055\000\056\000\056\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\056\000\058\000\058\000\058\000\058\000\000\000\
\000\000\000\000\058\000\000\000\000\000\056\000\000\000\058\000\
\056\000\000\000\000\000\000\000\056\000\056\000\000\000\056\000\
\000\000\058\000\000\000\058\000\000\000\056\000\058\000\058\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\058\000\061\000\
\061\000\061\000\061\000\000\000\000\000\000\000\061\000\000\000\
\000\000\058\000\000\000\061\000\058\000\000\000\000\000\000\000\
\058\000\058\000\000\000\058\000\000\000\061\000\000\000\061\000\
\000\000\058\000\061\000\061\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\061\000\060\000\060\000\060\000\060\000\000\000\
\000\000\000\000\060\000\000\000\000\000\061\000\000\000\060\000\
\061\000\056\000\000\000\000\000\061\000\061\000\000\000\061\000\
\000\000\060\000\000\000\060\000\000\000\061\000\060\000\060\000\
\000\000\000\000\000\000\000\000\070\000\071\000\072\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\060\000\076\000\
\077\000\078\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\060\000\000\000\000\000\060\000\000\000\000\000\000\000\
\060\000\060\000\000\000\060\000\000\000\000\000\000\000\000\000\
\000\000\060\000\000\000\000\000\000\000\000\000\000\000\107\000"

let yycheck = "\006\000\
\000\000\032\001\009\000\116\000\117\000\118\000\001\001\044\001\
\001\000\004\001\160\000\018\000\007\001\008\001\031\001\052\001\
\084\001\012\001\007\001\008\001\170\000\016\001\029\000\032\001\
\019\001\020\001\021\001\027\001\005\001\006\001\025\001\026\001\
\035\001\028\001\032\001\030\001\031\001\032\001\033\001\031\001\
\017\001\009\001\052\001\074\001\033\001\013\001\035\001\031\001\
\069\001\162\000\027\001\032\001\029\001\005\001\007\001\008\001\
\024\001\064\000\053\001\012\001\055\001\056\001\057\001\031\001\
\059\001\074\001\051\001\062\001\044\001\069\001\065\001\048\001\
\079\000\080\000\081\000\070\001\071\001\072\001\052\001\032\001\
\033\001\076\001\077\001\078\001\079\001\080\001\081\001\082\001\
\052\001\066\001\067\001\059\001\069\001\001\001\062\001\102\000\
\004\001\044\001\075\001\007\001\008\001\044\001\052\001\027\001\
\012\001\066\001\059\001\039\001\016\001\062\001\032\001\019\001\
\020\001\021\001\052\001\083\001\048\001\025\001\026\001\048\001\
\028\001\069\001\030\001\031\001\032\001\033\001\067\001\044\001\
\135\000\136\000\137\000\138\000\139\000\140\000\066\001\067\001\
\033\001\059\001\039\001\032\001\062\001\148\000\006\001\017\001\
\017\001\053\001\153\000\055\001\056\001\057\001\052\001\059\001\
\074\001\051\001\062\001\051\001\027\001\065\001\039\001\029\001\
\048\001\168\000\070\001\071\001\072\001\051\001\066\001\066\001\
\076\001\077\001\078\001\079\001\080\001\081\001\082\001\004\001\
\067\001\031\001\007\001\008\001\075\001\048\001\067\001\012\001\
\017\001\075\001\052\001\016\001\066\001\069\001\019\001\020\001\
\021\001\069\001\029\001\067\001\074\000\026\001\066\001\028\001\
\007\000\066\001\131\000\032\001\033\001\094\000\149\000\172\000\
\162\000\102\000\100\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\007\001\008\001\255\255\255\255\
\053\001\012\001\055\001\056\001\057\001\255\255\059\001\255\255\
\255\255\062\001\007\001\008\001\065\001\255\255\255\255\012\001\
\255\255\070\001\071\001\072\001\255\255\032\001\033\001\076\001\
\077\001\078\001\079\001\080\001\081\001\082\001\255\255\005\001\
\006\001\007\001\008\001\032\001\033\001\255\255\012\001\255\255\
\051\001\255\255\255\255\017\001\255\255\255\255\255\255\061\000\
\059\001\063\000\255\255\062\001\255\255\027\001\051\001\029\001\
\255\255\255\255\032\001\033\001\255\255\255\255\059\001\255\255\
\255\255\062\001\255\255\255\255\082\000\083\000\084\000\255\255\
\069\001\255\255\048\001\005\001\006\001\007\001\008\001\093\000\
\255\255\255\255\012\001\255\255\255\255\059\001\255\255\017\001\
\062\001\255\255\104\000\255\255\066\001\067\001\255\255\069\001\
\255\255\027\001\255\255\029\001\114\000\075\001\032\001\033\001\
\255\255\255\255\255\255\255\255\255\255\027\001\255\255\255\255\
\255\255\255\255\255\255\255\255\084\001\255\255\048\001\005\001\
\006\001\007\001\008\001\255\255\255\255\255\255\012\001\255\255\
\255\255\059\001\048\001\017\001\062\001\051\001\052\001\255\255\
\066\001\067\001\255\255\069\001\255\255\027\001\255\255\029\001\
\255\255\075\001\032\001\033\001\066\001\067\001\255\255\069\001\
\255\255\255\255\255\255\255\255\255\255\075\001\255\255\255\255\
\255\255\255\255\048\001\005\001\006\001\007\001\008\001\255\255\
\255\255\255\255\012\001\255\255\255\255\059\001\255\255\017\001\
\062\001\255\255\255\255\255\255\066\001\067\001\255\255\069\001\
\255\255\027\001\255\255\029\001\255\255\075\001\032\001\033\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\048\001\005\001\
\006\001\007\001\008\001\255\255\255\255\255\255\012\001\255\255\
\255\255\059\001\255\255\017\001\062\001\255\255\255\255\255\255\
\066\001\067\001\255\255\069\001\255\255\027\001\255\255\029\001\
\255\255\075\001\032\001\033\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\048\001\005\001\006\001\007\001\008\001\255\255\
\255\255\255\255\012\001\255\255\255\255\059\001\255\255\017\001\
\062\001\255\255\255\255\255\255\066\001\067\001\255\255\069\001\
\255\255\027\001\255\255\029\001\255\255\075\001\032\001\033\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\048\001\005\001\
\006\001\007\001\008\001\255\255\255\255\255\255\012\001\255\255\
\255\255\059\001\255\255\017\001\062\001\255\255\255\255\255\255\
\066\001\067\001\255\255\069\001\255\255\027\001\255\255\029\001\
\255\255\075\001\032\001\033\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\048\001\005\001\006\001\007\001\008\001\255\255\
\255\255\255\255\012\001\255\255\255\255\059\001\255\255\017\001\
\062\001\015\000\255\255\255\255\066\001\067\001\255\255\069\001\
\255\255\027\001\255\255\029\001\255\255\075\001\032\001\033\001\
\255\255\255\255\255\255\255\255\034\000\035\000\036\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\048\001\045\000\
\046\000\047\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\059\001\255\255\255\255\062\001\255\255\255\255\255\255\
\066\001\067\001\255\255\069\001\255\255\255\255\255\255\255\255\
\255\255\075\001\255\255\255\255\255\255\255\255\255\255\077\000"

let yynames_const = "\
  "

let yynames_block = "\
  IMPORT\000\
  AS\000\
  USTRING\000\
  IF\000\
  THEN\000\
  ELSE\000\
  TRUE\000\
  FALSE\000\
  BOOL\000\
  CASE\000\
  OF\000\
  UNIT\000\
  UUNIT\000\
  TIMESFLOAT\000\
  UFLOAT\000\
  LET\000\
  IN\000\
  INERT\000\
  LAMBDA\000\
  FIX\000\
  LETREC\000\
  TYPE\000\
  ISZERO\000\
  INT\000\
  INDUCTIVE\000\
  MATCH\000\
  WITH\000\
  DASH\000\
  END\000\
  CONFIG\000\
  UCID\000\
  LCID\000\
  INTV\000\
  FLOATV\000\
  STRINGV\000\
  BINARYINTOP\000\
  APOSTROPHE\000\
  DQUOTE\000\
  ARROW\000\
  BANG\000\
  BARGT\000\
  BARRCURLY\000\
  BARRSQUARE\000\
  COLON\000\
  COLONCOLON\000\
  COLONEQ\000\
  COLONHASH\000\
  COMMA\000\
  DARROW\000\
  DDARROW\000\
  DOT\000\
  EOF\000\
  EQ\000\
  EQEQ\000\
  EXISTS\000\
  GT\000\
  LEQ\000\
  GEQ\000\
  HASH\000\
  LCURLY\000\
  LCURLYBAR\000\
  LEFTARROW\000\
  LPAREN\000\
  LSQUARE\000\
  LSQUAREBAR\000\
  LT\000\
  RCURLY\000\
  RPAREN\000\
  RSQUARE\000\
  SEMI\000\
  PLUS\000\
  SLASH\000\
  STAR\000\
  TRIANGLE\000\
  USCORE\000\
  VBAR\000\
  ALIGN\000\
  LABEL\000\
  UNLABEL\000\
  OR\000\
  AND\000\
  NOT\000\
  RDASH\000\
  COMPRESS\000\
  DECRO\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 143 "parser.mly"
      ( fun ctx -> [],ctx )
# 558 "parser.ml"
               :  Syntax.context -> ((string list * Syntax.command) list * Syntax.context) ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'DecroList) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 :  Syntax.context -> ((string list * Syntax.command) list * Syntax.context) ) in
    Obj.repr(
# 145 "parser.mly"
      ( fun ctx ->
          let cmd,ctx = _2 ctx in
          let cmds,ctx = _4 ctx in
          let deco = _1 in 
            List.append (List.map (fun c -> (deco, c)) cmd) cmds, ctx
      )
# 573 "parser.ml"
               :  Syntax.context -> ((string list * Syntax.command) list * Syntax.context) ))
; (fun __caml_parser_env ->
    Obj.repr(
# 154 "parser.mly"
      ([])
# 579 "parser.ml"
               : 'DecroList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'DecroList) in
    Obj.repr(
# 156 "parser.mly"
      (_2.v::_3)
# 588 "parser.ml"
               : 'DecroList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 161 "parser.mly"
                   ( fun ctx -> ([Import(_2.v)], ctx) )
# 596 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'CommandValue) in
    Obj.repr(
# 162 "parser.mly"
                                ( fun ctx -> ([Config(_2.v, _4)], ctx) )
# 606 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 164 "parser.mly"
      ( fun ctx -> let t = _1 ctx in ([Eval(tmInfo t, t)], ctx))
# 613 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 166 "parser.mly"
      ( fun ctx -> ([(Bind(_1.i,_1.v,_2 ctx))], addname ctx _1.v) )
# 621 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'TyBinder) in
    Obj.repr(
# 168 "parser.mly"
      ( fun ctx -> ([(Bind(_1.i, _1.v, _2 ctx))], addname ctx _1.v) )
# 629 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'ConsList) in
    Obj.repr(
# 170 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _2.v in
            let cons_list = _4 ctx1 in 
              let ctx2 = List.fold_left
                  (fun pctx -> fun (name, _) -> addname pctx name) ctx1 cons_list in
                    ([(DefInductive(_1, _2.v, cons_list))], ctx2)
      )
# 645 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ConsList) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : string Support.Error.withinfo) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'ConsList) in
    Obj.repr(
# 178 "parser.mly"
     (
       fun ctx ->
         let ctx' = addname ctx _2.v in 
         let ctx1 = addname ctx' _6.v in 
         let t2 = TyInductive (_6.v, _8 ctx1) in
         let mo cons_ty = 
         (
          let res = typeSubstTop t2 cons_ty in 
            res
         )
         in 
         let cons1 = List.map (fun (name, cons_ty) -> (name, mo cons_ty)) (_4 ctx1) in
         let c1 = DefInductive (_1, _2.v, cons1) in 
         let upd pctx cons_list = List.fold_left (fun pctx -> fun (name, _) -> addname pctx name) pctx cons_list in
         let ctx3 = upd (addname ctx _2.v) cons1 in 
         let ctx4 = addname ctx3 _6.v in 
         let cons2 = _8 ctx4 in 
         let c2 = DefInductive (_1, _6.v, cons2) in 
           ([c1; c2], upd ctx4 cons2)
     )
# 678 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 200 "parser.mly"
         (BoolConfig true)
# 685 "parser.ml"
               : 'CommandValue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 201 "parser.mly"
          (BoolConfig false)
# 692 "parser.ml"
               : 'CommandValue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 202 "parser.mly"
            (StringConfig _1.v)
# 699 "parser.ml"
               : 'CommandValue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int Support.Error.withinfo) in
    Obj.repr(
# 203 "parser.mly"
         (IntConfig _1.v)
# 706 "parser.ml"
               : 'CommandValue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ConsList) in
    Obj.repr(
# 207 "parser.mly"
      (((fun ctx -> (addname ctx _1.v, [_1.v])), fun ctx -> [_3 ctx]))
# 715 "parser.ml"
               : 'InductiveList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ConsList) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'InductiveList) in
    Obj.repr(
# 209 "parser.mly"
      (let (fname, fcons) = _5 in 
        (
          (fun ctx ->
            let ctx1 = addname ctx _1.v in 
              let (ctx2, names) = fname ctx1 in
                (ctx2, _1.v :: names)), 
          fun ctx ->
            let cons = _3 ctx in
              let conslist = fcons ctx in 
                cons :: conslist 
        )
      )
# 737 "parser.ml"
               : 'InductiveList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 224 "parser.mly"
      ( fun ctx -> [(_1.v, _2 ctx)] )
# 745 "parser.ml"
               : 'ConsList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'ConsList) in
    Obj.repr(
# 226 "parser.mly"
      ( fun ctx-> (_1.v, _2 ctx) :: (_4 ctx) )
# 755 "parser.ml"
               : 'ConsList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 231 "parser.mly"
      ( fun ctx -> VarBind (_2 ctx))
# 763 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 233 "parser.mly"
      ( fun ctx -> TmAbbBind(_2 ctx, None) )
# 771 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 238 "parser.mly"
                ( _1 )
# 778 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 243 "parser.mly"
           ( _2 )
# 787 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 245 "parser.mly"
      ( fun ctx -> TyBool )
# 794 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 247 "parser.mly"
      ( fun ctx -> TyUnit )
# 801 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 249 "parser.mly"
      ( fun ctx ->
          (if isnamebound ctx _1.v then ( 
            (*pr $1.v; pr " "; pr (string_of_int (ctxlength ctx)); 
            pr " "; pr (ctx2string ctx); pr "\n";*)
            TyVar(name2index _1.i ctx _1.v, ctxlength ctx))
          else 
            TyId(_1.v)) )
# 814 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'FieldTypes) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 257 "parser.mly"
      ( fun ctx ->
          TyTuple(_2 ctx) )
# 824 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 260 "parser.mly"
      ( fun ctx -> TyInt )
# 831 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'AType) in
    Obj.repr(
# 262 "parser.mly"
      ( fun ctx -> TyCompress (_2 ctx))
# 839 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 266 "parser.mly"
      ( fun ctx -> [_1 ctx] )
# 846 "parser.ml"
               : 'FieldTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'FieldTypes) in
    Obj.repr(
# 268 "parser.mly"
      ( fun ctx -> (_1 ctx) :: (_3 ctx) )
# 855 "parser.ml"
               : 'FieldTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'PathTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int Support.Error.withinfo) in
    Obj.repr(
# 272 "parser.mly"
      ( fun ctx ->
          TmProj(_2, _1 ctx, _3.v) )
# 865 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 274 "parser.mly"
          ( _1 )
# 872 "parser.ml"
               : 'PathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 280 "parser.mly"
     ( fun ctx -> TyArr(_1 ctx, _3 ctx) )
# 881 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AType) in
    Obj.repr(
# 282 "parser.mly"
            ( _1 )
# 888 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 286 "parser.mly"
      ( _1 )
# 895 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 288 "parser.mly"
      ( fun ctx -> TmIf(_1, _2 ctx, _4 ctx, _6 ctx) )
# 907 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 290 "parser.mly"
      ( fun ctx -> TmLet(_1, _2.v, _4 ctx, _6 (addname ctx _2.v)) )
# 919 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 292 "parser.mly"
      ( fun ctx -> TmLet(_1, "_", _4 ctx, _6 (addname ctx "_")) )
# 931 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 294 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _2.v in
          TmAbs(_1, _2.v, _4 ctx, _6 ctx1) )
# 945 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 298 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs(_1, "_", _4 ctx, _6 ctx1) )
# 959 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 302 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _2.v in
          TmAbs(_1, _2.v, _4 ctx, _6 ctx1) )
# 973 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 306 "parser.mly"
      ( fun ctx -> 
          let ctx1 = addname ctx _2.v in 
          TmLet(_1, _2.v, TmFix(_1, TmAbs(_1, _2.v, _4 ctx, _6 ctx1)),
                _8 ctx1) )
# 990 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'MatchCases) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 311 "parser.mly"
      ( fun ctx -> TmMatch(_1, _2 ctx, _4 ctx) )
# 1001 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Pattern) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 315 "parser.mly"
      ( fun ctx -> 
        let ctx1 = addpattern ctx _1 in [(_1, _3 ctx1)])
# 1011 "parser.ml"
               : 'MatchCases))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'Pattern) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'MatchCases) in
    Obj.repr(
# 318 "parser.mly"
      ( fun ctx -> 
          let ctx1 = addpattern ctx _1 in (_1, _3 ctx1) :: (_5 ctx))
# 1023 "parser.ml"
               : 'MatchCases))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 322 "parser.mly"
           (PtUnderScore _1)
# 1030 "parser.ml"
               : 'Pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 323 "parser.mly"
         (PtVar(_1.i, _1.v))
# 1037 "parser.ml"
               : 'Pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Patterns) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 324 "parser.mly"
                           (PtTuple (_1, _2))
# 1046 "parser.ml"
               : 'Pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Pattern) in
    Obj.repr(
# 325 "parser.mly"
                 (PtConstructor (_1.i, _1.v, _2))
# 1054 "parser.ml"
               : 'Pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 326 "parser.mly"
                          (_2)
# 1063 "parser.ml"
               : 'Pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Pattern) in
    Obj.repr(
# 329 "parser.mly"
            ([_1])
# 1070 "parser.ml"
               : 'Patterns))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Pattern) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Patterns) in
    Obj.repr(
# 330 "parser.mly"
                           (_1 :: _3)
# 1079 "parser.ml"
               : 'Patterns))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 334 "parser.mly"
      ( _1 )
# 1086 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 336 "parser.mly"
      ( fun ctx -> TmLabel(_1, _2 ctx) )
# 1094 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 338 "parser.mly"
      ( fun ctx -> TmUnLabel(_1, _2 ctx) )
# 1102 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 340 "parser.mly"
      ( fun ctx -> TmAlign(_1, _2 ctx) )
# 1110 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 342 "parser.mly"
      ( fun ctx ->
          let e1 = _1 ctx in
          let e2 = _2 ctx in
          TmApp(tmInfo e1,e1,e2) )
# 1121 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 347 "parser.mly"
      ( fun ctx ->
          TmFix(_1, _2 ctx) )
# 1130 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'BinaryOp) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'PathTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 350 "parser.mly"
      ( fun ctx -> let (info, op) = _1 in TmPrimaryOp(info, op, [_2 ctx; _3 ctx]))
# 1139 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'UnaryOp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'PathTerm) in
    Obj.repr(
# 352 "parser.mly"
      ( fun ctx -> let (info, op) = _1 in TmPrimaryOp(info, op, [_2 ctx]))
# 1147 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 356 "parser.mly"
      ((_1, ("not", [TyBool], TyBool,
        fun inp -> match inp with
          (fi, [TmTrue _]) -> TmFalse fi
        | (fi, _) -> TmTrue fi
      )))
# 1158 "parser.ml"
               : 'UnaryOp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 364 "parser.mly"
      ((_1, ("+", [TyInt; TyInt], TyInt, 
        fun inp -> match inp with
          (fi, [TmInt (_, x); TmInt (_, y)]) -> TmInt (fi, x + y)
        | _ -> TmInt (dummyinfo, 0) 
       )))
# 1169 "parser.ml"
               : 'BinaryOp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 370 "parser.mly"
      ((_1, ("-", [TyInt; TyInt], TyInt, 
        fun inp -> match inp with
          (fi, [TmInt (_, x); TmInt (_, y)]) -> TmInt (fi, x - y)
        | _ -> TmInt (dummyinfo, 0) 
       )))
# 1180 "parser.ml"
               : 'BinaryOp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 376 "parser.mly"
      ((_1, ("*", [TyInt; TyInt], TyInt,
        fun inp -> match inp with
          (fi, [TmInt (_, x); TmInt (_, y)]) -> TmInt (fi, x * y)
        | _ -> TmInt (dummyinfo, 0)
        )))
# 1191 "parser.ml"
               : 'BinaryOp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 382 "parser.mly"
      ((_1, ("/", [TyInt; TyInt], TyInt,
        fun inp -> match inp with
          (fi, [TmInt (_, x); TmInt (_, y)]) -> TmInt (fi, x / y)
        | _ -> TmInt (dummyinfo, 0)
        )))
# 1202 "parser.ml"
               : 'BinaryOp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 388 "parser.mly"
      ((_1, ("==", [TyInt; TyInt], TyBool,
        fun inp -> match inp with
          (fi, [TmInt (_, x); TmInt (_, y)]) -> if x = y then TmTrue fi else TmFalse fi
        | _ -> TmFalse dummyinfo
        )))
# 1213 "parser.ml"
               : 'BinaryOp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 394 "parser.mly"
      ((_1, ("<", [TyInt; TyInt], TyBool,
        fun inp -> match inp with
          (fi, [TmInt (_, x); TmInt (_, y)]) -> if x < y then TmTrue fi else TmFalse fi
        | _ -> TmFalse dummyinfo
      )))
# 1224 "parser.ml"
               : 'BinaryOp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 400 "parser.mly"
      ((_1, ("<=", [TyInt; TyInt], TyBool,
        fun inp -> match inp with
          (fi, [TmInt (_, x); TmInt (_, y)]) -> if x <= y then TmTrue fi else TmFalse fi
        | _ -> TmFalse dummyinfo
      )))
# 1235 "parser.ml"
               : 'BinaryOp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 406 "parser.mly"
      ((_1, (">=", [TyInt; TyInt], TyBool,
        fun inp -> match inp with
          (fi, [TmInt (_, x); TmInt (_, y)]) -> if x >= y then TmTrue fi else TmFalse fi
        | _ -> TmFalse dummyinfo
      )))
# 1246 "parser.ml"
               : 'BinaryOp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 412 "parser.mly"
      ((_1, (">", [TyInt; TyInt], TyBool,
        fun inp -> match inp with
          (fi, [TmInt (_, x); TmInt (_, y)]) -> if x > y then TmTrue fi else TmFalse fi
        | _ -> TmFalse dummyinfo
      )))
# 1257 "parser.ml"
               : 'BinaryOp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 418 "parser.mly"
      ((_1, ("and", [TyBool; TyBool], TyBool,
        fun inp -> match inp with
          (fi, [TmTrue _; TmTrue _]) -> TmTrue fi
        | (fi, _) -> TmFalse fi
      )))
# 1268 "parser.ml"
               : 'BinaryOp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 424 "parser.mly"
      ((_1, ("or", [TyBool; TyBool], TyBool,
        fun inp -> match inp with
          (fi, [TmFalse _; TmFalse _]) -> TmFalse fi
        | (fi, _) -> TmTrue fi
      )))
# 1279 "parser.ml"
               : 'BinaryOp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 432 "parser.mly"
      ( _1 )
# 1286 "parser.ml"
               : 'TermSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'TermSeq) in
    Obj.repr(
# 434 "parser.mly"
      ( fun ctx ->
          TmApp(_2, TmAbs(_2, "_", TyUnit, _3 (addname ctx "_")), _1 ctx) )
# 1296 "parser.ml"
               : 'TermSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'TermSeq) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 440 "parser.mly"
      ( _2 )
# 1305 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 442 "parser.mly"
      ( fun ctx -> TmTrue(_1) )
# 1312 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 444 "parser.mly"
      ( fun ctx -> TmFalse(_1) )
# 1319 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 446 "parser.mly"
      ( fun ctx -> TmUnit(_1) )
# 1326 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 448 "parser.mly"
      ( fun ctx ->
          TmVar(_1.i, name2index _1.i ctx _1.v, ctxlength ctx) )
# 1334 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 451 "parser.mly"
      ( fun ctx ->
          TmTuple(_1, _2 ctx) )
# 1344 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int Support.Error.withinfo) in
    Obj.repr(
# 454 "parser.mly"
      (fun ctx -> TmInt(_1.i, _1.v))
# 1351 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 458 "parser.mly"
      ( fun ctx -> [_1 ctx] )
# 1358 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Fields) in
    Obj.repr(
# 460 "parser.mly"
      ( fun ctx -> (_1 ctx) :: (_3 ctx) )
# 1367 "parser.ml"
               : 'Fields))
; (fun __caml_parser_env ->
    Obj.repr(
# 464 "parser.mly"
      ( fun ctx -> TyVarBind )
# 1373 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 466 "parser.mly"
      ( fun ctx -> TyAbbBind(_2 ctx) )
# 1381 "parser.ml"
               : 'TyBinder))
(* Entry toplevel *)
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
let toplevel (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  Syntax.context -> ((string list * Syntax.command) list * Syntax.context) )
