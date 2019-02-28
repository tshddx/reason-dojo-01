# 1 "Camomile/internal/uReStrParser.ml"
type token =
  | UCHAR of (UChar.t)
  | ASCII of (char)
  | DOT
  | ASTARISK
  | REPN of (int * (int option) * string)
  | PLUS
  | QUESTION
  | LEFT_BRACKET
  | RIGHT_BRACKET
  | MINUS
  | HAT
  | DOLLAR
  | ALT
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | SPACE
  | AND
  | OR
  | COLON
  | BOS
  | EOS
  | END

open Parsing;;
let _ = parse_error;;
# 37 "Camomile/internal/uReStrParser.mly"

let any =
  let excluded_chars = List.map UChar.chr_of_uint
      [0x2028; 0x2029; 0x0a; 0x0c; 0x0d; 0x85] in
  let s = List.fold_right USet.add excluded_chars USet.empty in
  USet.compl s

let line_separators =
  let cr = [UChar.chr_of_uint 0x0d] in
  let lf = [UChar.chr_of_uint 0x0a] in
  let crlf = cr @ lf in
  let ls = [UChar.chr_of_uint 0x2028] in
  let ps = [UChar.chr_of_uint 0x2029] in
  let ff = [UChar.chr_of_uint 0x0d] in
  let nel = [UChar.chr_of_uint 0x85] in
  let r = `String nel in
  let r = `Alt (`String ff, r) in
  let r = `Alt (`String lf, r) in
  let r = `Alt (`String cr, r) in
  let r = `Alt (`String crlf, r) in
  let r = `Alt (`String ps, r) in
  `Alt (`String ls, r)

let bol = `Alt (`BoS, `After line_separators)

let eol = `Alt (`EoS, `Before line_separators)

let string_of_list cs =
  let b = Buffer.create 0 in
  List.iter (Buffer.add_char b) cs;
  Buffer.contents b

let quoted_charset c =
  let s = USet.add (UChar.of_char '\\') USet.empty in
  USet.add (UChar.of_char c) s

let set_of_string s =
  let r = ref USet.empty in
  String.iter (fun c -> 
    r := USet.add (UChar.of_char c) !r)
    s;
  !r
  
# 74 "Camomile/internal/uReStrParser.ml"
let yytransl_const = [|
  259 (* DOT *);
  260 (* ASTARISK *);
  262 (* PLUS *);
  263 (* QUESTION *);
  264 (* LEFT_BRACKET *);
  265 (* RIGHT_BRACKET *);
  266 (* MINUS *);
  267 (* HAT *);
  268 (* DOLLAR *);
  269 (* ALT *);
  270 (* LEFT_PAREN *);
  271 (* RIGHT_PAREN *);
  272 (* LEFT_BRACE *);
  273 (* RIGHT_BRACE *);
  274 (* SPACE *);
  275 (* AND *);
  276 (* OR *);
  277 (* COLON *);
  278 (* BOS *);
  279 (* EOS *);
  280 (* END *);
    0|]

let yytransl_block = [|
  257 (* UCHAR *);
  258 (* ASCII *);
  261 (* REPN *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\003\000\003\000\003\000\003\000\003\000\003\000\007\000\
\007\000\007\000\007\000\007\000\008\000\008\000\008\000\008\000\
\008\000\008\000\009\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\005\000\005\000\006\000\006\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\012\000\012\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\001\000\001\000\001\000\002\000\002\000\
\002\000\002\000\003\000\002\000\003\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\002\000\002\000\001\000\001\000\001\000\
\003\000\002\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\002\000\002\000\002\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\017\000\018\000\004\000\000\000\019\000\005\000\
\006\000\000\000\020\000\021\000\022\000\023\000\024\000\025\000\
\015\000\016\000\002\000\072\000\000\000\014\000\044\000\045\000\
\049\000\046\000\039\000\047\000\048\000\051\000\057\000\056\000\
\000\000\050\000\042\000\037\000\038\000\000\000\052\000\053\000\
\054\000\055\000\040\000\041\000\000\000\000\000\030\000\031\000\
\058\000\000\000\007\000\008\000\009\000\010\000\000\000\001\000\
\000\000\059\000\000\000\000\000\035\000\036\000\060\000\000\000\
\000\000\000\000\000\000\000\000\061\000\003\000\000\000\000\000\
\013\000\000\000\000\000\000\000\071\000\000\000\000\000\069\000\
\000\000\043\000\068\000\000\000\000\000\000\000\027\000\033\000\
\062\000\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\020\000\057\000\045\000\022\000\046\000\059\000\076\000\061\000\
\062\000\068\000\063\000\069\000"

let yysindex = "\006\000\
\061\255\000\000\000\000\000\000\000\000\085\255\000\000\000\000\
\000\000\036\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\016\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\177\255\000\000\000\000\000\000\000\000\040\255\000\000\000\000\
\000\000\000\000\000\000\000\000\108\255\003\255\000\000\000\000\
\000\000\131\255\000\000\000\000\000\000\000\000\036\000\000\000\
\246\255\000\000\004\255\059\000\000\000\000\000\000\000\013\255\
\085\255\040\255\040\255\035\255\000\000\000\000\013\000\082\000\
\000\000\246\255\082\000\013\000\000\000\154\255\007\255\000\000\
\040\255\000\000\000\000\040\255\040\255\040\255\000\000\000\000\
\000\000\025\255\007\255\131\000\007\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\200\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\052\255\000\000\223\255\107\000\000\000\000\000\000\000\087\000\
\000\000\000\000\000\000\000\000\000\000\000\000\252\254\000\000\
\000\000\027\000\000\000\253\254\000\000\000\000\113\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\137\000\119\000\247\254\125\000"

let yygindex = "\000\000\
\000\000\002\000\222\255\000\000\000\000\231\255\227\255\250\255\
\251\255\029\001\252\255\233\255"

let yytablesize = 413
let yytable = "\047\000\
\048\000\049\000\021\000\060\000\029\000\034\000\001\000\065\000\
\029\000\034\000\065\000\050\000\072\000\075\000\064\000\071\000\
\003\000\004\000\005\000\051\000\052\000\053\000\054\000\006\000\
\083\000\007\000\008\000\009\000\055\000\010\000\078\000\011\000\
\012\000\013\000\014\000\015\000\016\000\017\000\018\000\056\000\
\077\000\064\000\083\000\084\000\081\000\086\000\087\000\065\000\
\071\000\088\000\066\000\082\000\083\000\084\000\085\000\086\000\
\074\000\067\000\047\000\048\000\049\000\003\000\004\000\005\000\
\012\000\000\000\012\000\000\000\006\000\000\000\007\000\008\000\
\009\000\000\000\010\000\012\000\011\000\012\000\013\000\014\000\
\015\000\016\000\017\000\018\000\019\000\023\000\024\000\025\000\
\026\000\027\000\028\000\029\000\030\000\031\000\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\000\000\039\000\040\000\
\041\000\042\000\043\000\044\000\023\000\024\000\025\000\026\000\
\027\000\028\000\029\000\030\000\070\000\032\000\058\000\034\000\
\035\000\036\000\037\000\038\000\000\000\039\000\040\000\041\000\
\042\000\043\000\044\000\003\000\004\000\005\000\051\000\052\000\
\053\000\054\000\006\000\000\000\007\000\008\000\009\000\055\000\
\010\000\073\000\011\000\012\000\013\000\014\000\015\000\016\000\
\017\000\018\000\023\000\024\000\025\000\026\000\027\000\028\000\
\029\000\030\000\089\000\032\000\058\000\034\000\035\000\036\000\
\037\000\038\000\000\000\039\000\040\000\041\000\042\000\043\000\
\044\000\023\000\024\000\025\000\026\000\027\000\028\000\029\000\
\030\000\000\000\032\000\058\000\034\000\035\000\036\000\037\000\
\038\000\000\000\039\000\040\000\041\000\042\000\043\000\044\000\
\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
\026\000\000\000\026\000\026\000\026\000\026\000\026\000\026\000\
\000\000\026\000\026\000\026\000\026\000\026\000\026\000\032\000\
\032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
\000\000\032\000\032\000\032\000\032\000\032\000\032\000\000\000\
\032\000\032\000\032\000\032\000\032\000\032\000\003\000\004\000\
\005\000\051\000\052\000\053\000\054\000\006\000\000\000\007\000\
\008\000\009\000\000\000\010\000\000\000\011\000\012\000\013\000\
\014\000\015\000\016\000\017\000\018\000\023\000\024\000\025\000\
\026\000\027\000\028\000\029\000\030\000\000\000\032\000\058\000\
\034\000\000\000\036\000\037\000\038\000\000\000\039\000\040\000\
\041\000\042\000\043\000\044\000\003\000\004\000\005\000\011\000\
\000\000\011\000\000\000\006\000\000\000\007\000\008\000\009\000\
\000\000\010\000\011\000\011\000\012\000\013\000\014\000\015\000\
\016\000\017\000\018\000\023\000\024\000\025\000\026\000\027\000\
\028\000\029\000\030\000\000\000\000\000\000\000\034\000\000\000\
\000\000\000\000\038\000\000\000\039\000\000\000\000\000\000\000\
\043\000\044\000\023\000\024\000\025\000\026\000\000\000\028\000\
\029\000\030\000\000\000\032\000\058\000\034\000\079\000\080\000\
\070\000\000\000\000\000\039\000\040\000\041\000\042\000\070\000\
\070\000\070\000\070\000\070\000\000\000\090\000\000\000\000\000\
\091\000\092\000\093\000\028\000\028\000\028\000\000\000\028\000\
\028\000\028\000\067\000\000\000\000\000\028\000\028\000\028\000\
\064\000\067\000\000\000\067\000\067\000\067\000\063\000\064\000\
\000\000\064\000\064\000\064\000\081\000\063\000\000\000\063\000\
\063\000\063\000\066\000\000\000\083\000\084\000\000\000\086\000\
\000\000\066\000\000\000\000\000\066\000"

let yycheck = "\006\000\
\006\000\006\000\001\000\033\000\009\001\009\001\001\000\017\001\
\013\001\013\001\020\001\010\000\010\001\010\001\002\001\045\000\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\008\001\
\018\001\010\001\011\001\012\001\013\001\014\001\065\000\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\064\000\002\001\018\001\019\001\010\001\021\001\072\000\008\001\
\078\000\075\000\011\001\017\001\018\001\019\001\020\001\021\001\
\055\000\018\001\065\000\065\000\065\000\001\001\002\001\003\001\
\013\001\255\255\015\001\255\255\008\001\255\255\010\001\011\001\
\012\001\255\255\014\001\024\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\255\255\018\001\019\001\
\020\001\021\001\022\001\023\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\255\255\018\001\019\001\020\001\
\021\001\022\001\023\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\255\255\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\001\001\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\255\255\018\001\019\001\020\001\021\001\022\001\
\023\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\255\255\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\255\255\018\001\019\001\020\001\021\001\022\001\023\001\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\008\001\
\009\001\255\255\011\001\012\001\013\001\014\001\015\001\016\001\
\255\255\018\001\019\001\020\001\021\001\022\001\023\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\255\255\011\001\012\001\013\001\014\001\015\001\016\001\255\255\
\018\001\019\001\020\001\021\001\022\001\023\001\001\001\002\001\
\003\001\004\001\005\001\006\001\007\001\008\001\255\255\010\001\
\011\001\012\001\255\255\014\001\255\255\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\255\255\010\001\011\001\
\012\001\255\255\014\001\015\001\016\001\255\255\018\001\019\001\
\020\001\021\001\022\001\023\001\001\001\002\001\003\001\013\001\
\255\255\015\001\255\255\008\001\255\255\010\001\011\001\012\001\
\255\255\014\001\024\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\255\255\255\255\255\255\012\001\255\255\
\255\255\255\255\016\001\255\255\018\001\255\255\255\255\255\255\
\022\001\023\001\001\001\002\001\003\001\004\001\255\255\006\001\
\007\001\008\001\255\255\010\001\011\001\012\001\066\000\067\000\
\010\001\255\255\255\255\018\001\019\001\020\001\021\001\017\001\
\018\001\019\001\020\001\021\001\255\255\081\000\255\255\255\255\
\084\000\085\000\086\000\009\001\010\001\011\001\255\255\013\001\
\014\001\015\001\010\001\255\255\255\255\019\001\020\001\021\001\
\010\001\017\001\255\255\019\001\020\001\021\001\010\001\017\001\
\255\255\019\001\020\001\021\001\010\001\017\001\255\255\019\001\
\020\001\021\001\010\001\255\255\018\001\019\001\255\255\021\001\
\255\255\017\001\255\255\255\255\020\001"

let yynames_const = "\
  DOT\000\
  ASTARISK\000\
  PLUS\000\
  QUESTION\000\
  LEFT_BRACKET\000\
  RIGHT_BRACKET\000\
  MINUS\000\
  HAT\000\
  DOLLAR\000\
  ALT\000\
  LEFT_PAREN\000\
  RIGHT_PAREN\000\
  LEFT_BRACE\000\
  RIGHT_BRACE\000\
  SPACE\000\
  AND\000\
  OR\000\
  COLON\000\
  BOS\000\
  EOS\000\
  END\000\
  "

let yynames_block = "\
  UCHAR\000\
  ASCII\000\
  REPN\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'regexp) in
    Obj.repr(
# 123 "Camomile/internal/uReStrParser.mly"
             (_1)
# 323 "Camomile/internal/uReStrParser.ml"
               : UReStrParserType.tree))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "Camomile/internal/uReStrParser.mly"
      (`Epsilon)
# 329 "Camomile/internal/uReStrParser.ml"
               : UReStrParserType.tree))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'charset) in
    Obj.repr(
# 127 "Camomile/internal/uReStrParser.mly"
                                     (`SetNotation _2)
# 336 "Camomile/internal/uReStrParser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "Camomile/internal/uReStrParser.mly"
      (`Set any)
# 342 "Camomile/internal/uReStrParser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "Camomile/internal/uReStrParser.mly"
      (bol)
# 348 "Camomile/internal/uReStrParser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 130 "Camomile/internal/uReStrParser.mly"
         (eol)
# 354 "Camomile/internal/uReStrParser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'regexp) in
    Obj.repr(
# 131 "Camomile/internal/uReStrParser.mly"
                  (`Rep _1)
# 361 "Camomile/internal/uReStrParser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'regexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int * (int option) * string) in
    Obj.repr(
# 132 "Camomile/internal/uReStrParser.mly"
              (let n, m, _ = _2 in `Repn (_1, n, m))
# 369 "Camomile/internal/uReStrParser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'regexp) in
    Obj.repr(
# 133 "Camomile/internal/uReStrParser.mly"
              (`Seq (_1, (`Rep _1)))
# 376 "Camomile/internal/uReStrParser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'regexp) in
    Obj.repr(
# 134 "Camomile/internal/uReStrParser.mly"
                  (`Alt (_1, `Epsilon))
# 383 "Camomile/internal/uReStrParser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'regexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'regexp) in
    Obj.repr(
# 135 "Camomile/internal/uReStrParser.mly"
                    (`Alt (_1, _3))
# 391 "Camomile/internal/uReStrParser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'regexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'regexp) in
    Obj.repr(
# 136 "Camomile/internal/uReStrParser.mly"
                             (`Seq (_1, _2))
# 399 "Camomile/internal/uReStrParser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'regexp) in
    Obj.repr(
# 137 "Camomile/internal/uReStrParser.mly"
                                (`Group _2)
# 406 "Camomile/internal/uReStrParser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'uchar) in
    Obj.repr(
# 138 "Camomile/internal/uReStrParser.mly"
        (`String [_1])
# 413 "Camomile/internal/uReStrParser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 139 "Camomile/internal/uReStrParser.mly"
      (`BoS)
# 419 "Camomile/internal/uReStrParser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "Camomile/internal/uReStrParser.mly"
      (`EoS)
# 425 "Camomile/internal/uReStrParser.ml"
               : 'regexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : UChar.t) in
    Obj.repr(
# 143 "Camomile/internal/uReStrParser.mly"
        (_1)
# 432 "Camomile/internal/uReStrParser.ml"
               : 'uchar))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 144 "Camomile/internal/uReStrParser.mly"
        (UChar.of_char _1)
# 439 "Camomile/internal/uReStrParser.ml"
               : 'uchar))
; (fun __caml_parser_env ->
    Obj.repr(
# 145 "Camomile/internal/uReStrParser.mly"
        (UChar.of_char '-')
# 445 "Camomile/internal/uReStrParser.ml"
               : 'uchar))
; (fun __caml_parser_env ->
    Obj.repr(
# 146 "Camomile/internal/uReStrParser.mly"
             (UChar.of_char '{')
# 451 "Camomile/internal/uReStrParser.ml"
               : 'uchar))
; (fun __caml_parser_env ->
    Obj.repr(
# 147 "Camomile/internal/uReStrParser.mly"
              (UChar.of_char '}')
# 457 "Camomile/internal/uReStrParser.ml"
               : 'uchar))
; (fun __caml_parser_env ->
    Obj.repr(
# 148 "Camomile/internal/uReStrParser.mly"
        (UChar.of_char ' ')
# 463 "Camomile/internal/uReStrParser.ml"
               : 'uchar))
; (fun __caml_parser_env ->
    Obj.repr(
# 149 "Camomile/internal/uReStrParser.mly"
      (UChar.of_char '&')
# 469 "Camomile/internal/uReStrParser.ml"
               : 'uchar))
; (fun __caml_parser_env ->
    Obj.repr(
# 150 "Camomile/internal/uReStrParser.mly"
     (UChar.of_char '|')
# 475 "Camomile/internal/uReStrParser.ml"
               : 'uchar))
; (fun __caml_parser_env ->
    Obj.repr(
# 151 "Camomile/internal/uReStrParser.mly"
        (UChar.of_char ':')
# 481 "Camomile/internal/uReStrParser.ml"
               : 'uchar))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'head_charset_char) in
    Obj.repr(
# 154 "Camomile/internal/uReStrParser.mly"
                    (`Set (USet.add _1 USet.empty))
# 488 "Camomile/internal/uReStrParser.ml"
               : 'charset))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'head_charset_char) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'charset_char) in
    Obj.repr(
# 155 "Camomile/internal/uReStrParser.mly"
                                       (`Set (USet.add_range _1 _3 USet.empty))
# 496 "Camomile/internal/uReStrParser.ml"
               : 'charset))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'tail_charset) in
    Obj.repr(
# 156 "Camomile/internal/uReStrParser.mly"
                   (`Compl _2)
# 503 "Camomile/internal/uReStrParser.ml"
               : 'charset))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'charset) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'tail_charset) in
    Obj.repr(
# 157 "Camomile/internal/uReStrParser.mly"
                                    (`Union (_1, _2))
# 511 "Camomile/internal/uReStrParser.ml"
               : 'charset))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unquote) in
    Obj.repr(
# 158 "Camomile/internal/uReStrParser.mly"
          (`Set _1)
# 518 "Camomile/internal/uReStrParser.ml"
               : 'charset))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'set) in
    Obj.repr(
# 159 "Camomile/internal/uReStrParser.mly"
      (_1)
# 525 "Camomile/internal/uReStrParser.ml"
               : 'charset))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'charset_char) in
    Obj.repr(
# 162 "Camomile/internal/uReStrParser.mly"
               (`Set (USet.add _1 USet.empty))
# 532 "Camomile/internal/uReStrParser.ml"
               : 'tail_charset))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'charset_char) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'charset_char) in
    Obj.repr(
# 164 "Camomile/internal/uReStrParser.mly"
    (`Set (USet.add_range _1 _3 USet.empty))
# 540 "Camomile/internal/uReStrParser.ml"
               : 'tail_charset))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'tail_charset) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'tail_charset) in
    Obj.repr(
# 165 "Camomile/internal/uReStrParser.mly"
                                         (`Union (_1, _2))
# 548 "Camomile/internal/uReStrParser.ml"
               : 'tail_charset))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unquote) in
    Obj.repr(
# 166 "Camomile/internal/uReStrParser.mly"
          (`Set _1)
# 555 "Camomile/internal/uReStrParser.ml"
               : 'tail_charset))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'set) in
    Obj.repr(
# 167 "Camomile/internal/uReStrParser.mly"
      (_1)
# 562 "Camomile/internal/uReStrParser.ml"
               : 'tail_charset))
; (fun __caml_parser_env ->
    Obj.repr(
# 170 "Camomile/internal/uReStrParser.mly"
             (quoted_charset '(')
# 568 "Camomile/internal/uReStrParser.ml"
               : 'unquote))
; (fun __caml_parser_env ->
    Obj.repr(
# 171 "Camomile/internal/uReStrParser.mly"
              (quoted_charset ')')
# 574 "Camomile/internal/uReStrParser.ml"
               : 'unquote))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int * (int option) * string) in
    Obj.repr(
# 172 "Camomile/internal/uReStrParser.mly"
       (let _, _, s = _1 in set_of_string s)
# 581 "Camomile/internal/uReStrParser.ml"
               : 'unquote))
; (fun __caml_parser_env ->
    Obj.repr(
# 173 "Camomile/internal/uReStrParser.mly"
      (quoted_charset '`')
# 587 "Camomile/internal/uReStrParser.ml"
               : 'unquote))
; (fun __caml_parser_env ->
    Obj.repr(
# 174 "Camomile/internal/uReStrParser.mly"
      (quoted_charset '\'')
# 593 "Camomile/internal/uReStrParser.ml"
               : 'unquote))
; (fun __caml_parser_env ->
    Obj.repr(
# 175 "Camomile/internal/uReStrParser.mly"
      (quoted_charset '|')
# 599 "Camomile/internal/uReStrParser.ml"
               : 'unquote))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'set_notation) in
    Obj.repr(
# 178 "Camomile/internal/uReStrParser.mly"
                                      (_2)
# 606 "Camomile/internal/uReStrParser.ml"
               : 'set))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : UChar.t) in
    Obj.repr(
# 181 "Camomile/internal/uReStrParser.mly"
        (_1)
# 613 "Camomile/internal/uReStrParser.ml"
               : 'common_charset_char))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 182 "Camomile/internal/uReStrParser.mly"
        (UChar.of_char _1)
# 620 "Camomile/internal/uReStrParser.ml"
               : 'common_charset_char))
; (fun __caml_parser_env ->
    Obj.repr(
# 183 "Camomile/internal/uReStrParser.mly"
           (UChar.of_char '*')
# 626 "Camomile/internal/uReStrParser.ml"
               : 'common_charset_char))
; (fun __caml_parser_env ->
    Obj.repr(
# 184 "Camomile/internal/uReStrParser.mly"
       (UChar.of_char '+')
# 632 "Camomile/internal/uReStrParser.ml"
               : 'common_charset_char))
; (fun __caml_parser_env ->
    Obj.repr(
# 185 "Camomile/internal/uReStrParser.mly"
           (UChar.of_char '?')
# 638 "Camomile/internal/uReStrParser.ml"
               : 'common_charset_char))
; (fun __caml_parser_env ->
    Obj.repr(
# 186 "Camomile/internal/uReStrParser.mly"
      (UChar.of_char '.')
# 644 "Camomile/internal/uReStrParser.ml"
               : 'common_charset_char))
; (fun __caml_parser_env ->
    Obj.repr(
# 187 "Camomile/internal/uReStrParser.mly"
         (UChar.of_char '$')
# 650 "Camomile/internal/uReStrParser.ml"
               : 'common_charset_char))
; (fun __caml_parser_env ->
    Obj.repr(
# 188 "Camomile/internal/uReStrParser.mly"
               (UChar.of_char '[')
# 656 "Camomile/internal/uReStrParser.ml"
               : 'common_charset_char))
; (fun __caml_parser_env ->
    Obj.repr(
# 189 "Camomile/internal/uReStrParser.mly"
        (UChar.of_char ' ')
# 662 "Camomile/internal/uReStrParser.ml"
               : 'common_charset_char))
; (fun __caml_parser_env ->
    Obj.repr(
# 190 "Camomile/internal/uReStrParser.mly"
      (UChar.of_char '&')
# 668 "Camomile/internal/uReStrParser.ml"
               : 'common_charset_char))
; (fun __caml_parser_env ->
    Obj.repr(
# 191 "Camomile/internal/uReStrParser.mly"
     (UChar.of_char '|')
# 674 "Camomile/internal/uReStrParser.ml"
               : 'common_charset_char))
; (fun __caml_parser_env ->
    Obj.repr(
# 192 "Camomile/internal/uReStrParser.mly"
        (UChar.of_char ':')
# 680 "Camomile/internal/uReStrParser.ml"
               : 'common_charset_char))
; (fun __caml_parser_env ->
    Obj.repr(
# 193 "Camomile/internal/uReStrParser.mly"
        (UChar.of_char '-')
# 686 "Camomile/internal/uReStrParser.ml"
               : 'common_charset_char))
; (fun __caml_parser_env ->
    Obj.repr(
# 196 "Camomile/internal/uReStrParser.mly"
                (UChar.of_char ']')
# 692 "Camomile/internal/uReStrParser.ml"
               : 'head_charset_char))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'common_charset_char) in
    Obj.repr(
# 197 "Camomile/internal/uReStrParser.mly"
                      (_1)
# 699 "Camomile/internal/uReStrParser.ml"
               : 'head_charset_char))
; (fun __caml_parser_env ->
    Obj.repr(
# 200 "Camomile/internal/uReStrParser.mly"
      (UChar.of_char '^')
# 705 "Camomile/internal/uReStrParser.ml"
               : 'charset_char))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'common_charset_char) in
    Obj.repr(
# 201 "Camomile/internal/uReStrParser.mly"
                      (_1)
# 712 "Camomile/internal/uReStrParser.ml"
               : 'charset_char))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'property) in
    Obj.repr(
# 205 "Camomile/internal/uReStrParser.mly"
    (let name = string_of_list _1 in `Property name)
# 719 "Camomile/internal/uReStrParser.ml"
               : 'set_notation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'charset) in
    Obj.repr(
# 206 "Camomile/internal/uReStrParser.mly"
                                     (_2)
# 726 "Camomile/internal/uReStrParser.ml"
               : 'set_notation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'set_notation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'set_notation) in
    Obj.repr(
# 207 "Camomile/internal/uReStrParser.mly"
                                  (`Intr (_1, _3))
# 734 "Camomile/internal/uReStrParser.ml"
               : 'set_notation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'set_notation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'set_notation) in
    Obj.repr(
# 208 "Camomile/internal/uReStrParser.mly"
                                (`Intr (_1, _3))
# 742 "Camomile/internal/uReStrParser.ml"
               : 'set_notation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'set_notation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'set_notation) in
    Obj.repr(
# 209 "Camomile/internal/uReStrParser.mly"
                               (`Union (_1, _3))
# 750 "Camomile/internal/uReStrParser.ml"
               : 'set_notation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'set_notation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'set_notation) in
    Obj.repr(
# 210 "Camomile/internal/uReStrParser.mly"
                                  (`Diff (_1, _3))
# 758 "Camomile/internal/uReStrParser.ml"
               : 'set_notation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'set_notation) in
    Obj.repr(
# 211 "Camomile/internal/uReStrParser.mly"
                   (`Compl _2)
# 765 "Camomile/internal/uReStrParser.ml"
               : 'set_notation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'set_notation) in
    Obj.repr(
# 212 "Camomile/internal/uReStrParser.mly"
                     (_1)
# 772 "Camomile/internal/uReStrParser.ml"
               : 'set_notation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'set_notation) in
    Obj.repr(
# 213 "Camomile/internal/uReStrParser.mly"
                     (_2)
# 779 "Camomile/internal/uReStrParser.ml"
               : 'set_notation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 216 "Camomile/internal/uReStrParser.mly"
        ([_1])
# 786 "Camomile/internal/uReStrParser.ml"
               : 'property))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : char) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'property) in
    Obj.repr(
# 217 "Camomile/internal/uReStrParser.mly"
                 (_1 :: _2)
# 794 "Camomile/internal/uReStrParser.ml"
               : 'property))
(* Entry start *)
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
let start (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : UReStrParserType.tree)
