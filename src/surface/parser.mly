/*  
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Support.Error
open Support.Pervasive
open Syntax
%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of identifiers and
   constant values -- more information is provided.
 */

/* Keyword tokens */
%token <Support.Error.info> IMPORT
%token <Support.Error.info> AS
%token <Support.Error.info> USTRING
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> BOOL
%token <Support.Error.info> CASE
%token <Support.Error.info> OF
%token <Support.Error.info> UNIT
%token <Support.Error.info> UUNIT
%token <Support.Error.info> TIMESFLOAT
%token <Support.Error.info> UFLOAT
%token <Support.Error.info> LET
%token <Support.Error.info> IN
%token <Support.Error.info> INERT
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> FIX
%token <Support.Error.info> LETREC
%token <Support.Error.info> TYPE
%token <Support.Error.info> ISZERO
%token <Support.Error.info> INT
%token <Support.Error.info> INDUCTIVE
%token <Support.Error.info> MATCH
%token <Support.Error.info> WITH
%token <Support.Error.info> DASH
%token <Support.Error.info> END
%token <Support.Error.info> CONFIG

/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string Support.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int Support.Error.withinfo> INTV
%token <float Support.Error.withinfo> FLOATV
%token <string Support.Error.withinfo> STRINGV
%token <string Support.Error.withinfo> BINARYINTOP

/* Symbolic tokens */
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> DQUOTE
%token <Support.Error.info> ARROW
%token <Support.Error.info> BANG
%token <Support.Error.info> BARGT
%token <Support.Error.info> BARRCURLY
%token <Support.Error.info> BARRSQUARE
%token <Support.Error.info> COLON
%token <Support.Error.info> COLONCOLON
%token <Support.Error.info> COLONEQ
%token <Support.Error.info> COLONHASH
%token <Support.Error.info> COMMA
%token <Support.Error.info> DARROW
%token <Support.Error.info> DDARROW
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> EQEQ
%token <Support.Error.info> EXISTS
%token <Support.Error.info> GT
%token <Support.Error.info> LEQ 
%token <Support.Error.info> GEQ
%token <Support.Error.info> HASH
%token <Support.Error.info> LCURLY
%token <Support.Error.info> LCURLYBAR
%token <Support.Error.info> LEFTARROW
%token <Support.Error.info> LPAREN
%token <Support.Error.info> LSQUARE
%token <Support.Error.info> LSQUAREBAR
%token <Support.Error.info> LT
%token <Support.Error.info> RCURLY
%token <Support.Error.info> RPAREN
%token <Support.Error.info> RSQUARE
%token <Support.Error.info> SEMI
%token <Support.Error.info> PLUS
%token <Support.Error.info> SLASH
%token <Support.Error.info> STAR
%token <Support.Error.info> TRIANGLE
%token <Support.Error.info> USCORE
%token <Support.Error.info> VBAR
%token <Support.Error.info> ALIGN
%token <Support.Error.info> LABEL
%token <Support.Error.info> UNLABEL
%token <Support.Error.info> OR
%token <Support.Error.info> AND
%token <Support.Error.info> NOT
%token <Support.Error.info> RDASH
%token <Support.Error.info> COMPRESS
%token <Support.Error.info> DECRO


/* ---------------------------------------------------------------------- */
/* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
     Syntax.context -> (Syntax.command list * Syntax.context) 
   that is, the parser returns to the user program a function that,
   when given a naming context, returns a fully parsed list of
   Syntax.commands and the new naming context that results when
   all the names bound in these commands are defined.

   All of the syntactic productions in the parser follow the same pattern:
   they take a context as argument and return a fully parsed abstract
   syntax tree (and, if they involve any constructs that bind variables
   in some following phrase, a new context).
   
*/

%start toplevel
%type < Syntax.context -> ((string list * Syntax.command) list * Syntax.context) > toplevel
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
toplevel :
    EOF
      { fun ctx -> [],ctx }
  | DecroList Command SEMI toplevel
      { fun ctx ->
          let cmd,ctx = $2 ctx in
          let cmds,ctx = $4 ctx in
          let deco = $1 in 
            List.append (List.map (fun c -> (deco, c)) cmd) cmds, ctx
      }

DecroList:
    /* empty */
      {[]}
  | DECRO UCID DecroList
      {$2.v::$3}
    

/* A top-level command */
Command :
    IMPORT STRINGV { fun ctx -> ([Import($2.v)], ctx) }
  | CONFIG UCID EQ CommandValue { fun ctx -> ([Config($2.v, $4)], ctx) }
  | Term 
      { fun ctx -> let t = $1 ctx in ([Eval(tmInfo t, t)], ctx)}
  | LCID Binder
      { fun ctx -> ([(Bind($1.i,$1.v,$2 ctx))], addname ctx $1.v) }
  | UCID TyBinder
      { fun ctx -> ([(Bind($1.i, $1.v, $2 ctx))], addname ctx $1.v) }
  | INDUCTIVE UCID EQ ConsList
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
            let cons_list = $4 ctx1 in 
              let ctx2 = List.fold_left
                  (fun pctx -> fun (name, _) -> addname pctx name) ctx1 cons_list in
                    ([(DefInductive($1, $2.v, cons_list))], ctx2)
      }
  | INDUCTIVE UCID EQ ConsList WITH UCID EQ ConsList 
     {
       fun ctx ->
         let ctx' = addname ctx $2.v in 
         let ctx1 = addname ctx' $6.v in 
         let t2 = TyInductive ($6.v, $8 ctx1) in
         let mo cons_ty = 
         (
          let res = typeSubstTop t2 cons_ty in 
            res
         )
         in 
         let cons1 = List.map (fun (name, cons_ty) -> (name, mo cons_ty)) ($4 ctx1) in
         let c1 = DefInductive ($1, $2.v, cons1) in 
         let upd pctx cons_list = List.fold_left (fun pctx -> fun (name, _) -> addname pctx name) pctx cons_list in
         let ctx3 = upd (addname ctx $2.v) cons1 in 
         let ctx4 = addname ctx3 $6.v in 
         let cons2 = $8 ctx4 in 
         let c2 = DefInductive ($1, $6.v, cons2) in 
           ([c1; c2], upd ctx4 cons2)
     }

CommandValue :
    TRUE {BoolConfig true}
  | FALSE {BoolConfig false}
  | STRINGV {StringConfig $1.v}
  | INTV {IntConfig $1.v}

InductiveList : 
    UCID EQ ConsList 
      {((fun ctx -> (addname ctx $1.v, [$1.v])), fun ctx -> [$3 ctx])}
  | UCID EQ ConsList WITH InductiveList 
      {let (fname, fcons) = $5 in 
        (
          (fun ctx ->
            let ctx1 = addname ctx $1.v in 
              let (ctx2, names) = fname ctx1 in
                (ctx2, $1.v :: names)), 
          fun ctx ->
            let cons = $3 ctx in
              let conslist = fcons ctx in 
                cons :: conslist 
        )
      }

ConsList :
    LCID Type
      { fun ctx -> [($1.v, $2 ctx)] }  
  | LCID Type VBAR ConsList
      { fun ctx-> ($1.v, $2 ctx) :: ($4 ctx) }

/* Right-hand sides of top-level bindings */
Binder :
    COLON Type
      { fun ctx -> VarBind ($2 ctx)}
  | EQ Term 
      { fun ctx -> TmAbbBind($2 ctx, None) }

/* All type expressions */
Type :
    ArrowType
                { $1 }

/* Atomic types are those that never need extra parentheses */
AType :
    LPAREN Type RPAREN  
           { $2 } 
  | BOOL
      { fun ctx -> TyBool }
  | UUNIT
      { fun ctx -> TyUnit }
  | UCID 
      { fun ctx ->
          (if isnamebound ctx $1.v then ( 
            (*pr $1.v; pr " "; pr (string_of_int (ctxlength ctx)); 
            pr " "; pr (ctx2string ctx); pr "\n";*)
            TyVar(name2index $1.i ctx $1.v, ctxlength ctx))
          else 
            TyId($1.v)) }
  | LCURLY FieldTypes RCURLY
      { fun ctx ->
          TyTuple($2 ctx) }
  | INT
      { fun ctx -> TyInt }
  | COMPRESS AType
      { fun ctx -> TyCompress ($2 ctx)}

FieldTypes :
    Type
      { fun ctx -> [$1 ctx] }
  | Type COMMA FieldTypes
      { fun ctx -> ($1 ctx) :: ($3 ctx) }

PathTerm :
    PathTerm DOT INTV
      { fun ctx ->
          TmProj($2, $1 ctx, $3.v) }
  | ATerm { $1 }

/* An "arrow type" is a sequence of atomic types separated by
   arrows. */
ArrowType :
    AType ARROW ArrowType
     { fun ctx -> TyArr($1 ctx, $3 ctx) }
  | AType
            { $1 }

Term :
    AppTerm
      { $1 }
  | IF Term THEN Term ELSE Term
      { fun ctx -> TmIf($1, $2 ctx, $4 ctx, $6 ctx) }
  | LET LCID EQ Term IN Term
      { fun ctx -> TmLet($1, $2.v, $4 ctx, $6 (addname ctx $2.v)) }
  | LET USCORE EQ Term IN Term
      { fun ctx -> TmLet($1, "_", $4 ctx, $6 (addname ctx "_")) }
  | LAMBDA LCID COLON Type DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmAbs($1, $2.v, $4 ctx, $6 ctx1) }
  | LAMBDA USCORE COLON Type DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs($1, "_", $4 ctx, $6 ctx1) }
  | RDASH LCID COLON Type DOT Term
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmAbs($1, $2.v, $4 ctx, $6 ctx1) }
  | LETREC LCID COLON Type EQ Term IN Term
      { fun ctx -> 
          let ctx1 = addname ctx $2.v in 
          TmLet($1, $2.v, TmFix($1, TmAbs($1, $2.v, $4 ctx, $6 ctx1)),
                $8 ctx1) }
  | MATCH Term WITH MatchCases END
      { fun ctx -> TmMatch($1, $2 ctx, $4 ctx) }

MatchCases :
    Pattern ARROW Term 
      { fun ctx -> 
        let ctx1 = addpattern ctx $1 in [($1, $3 ctx1)]}
  | Pattern ARROW Term VBAR MatchCases
      { fun ctx -> 
          let ctx1 = addpattern ctx $1 in ($1, $3 ctx1) :: ($5 ctx)}

Pattern :
    USCORE {PtUnderScore $1}
  | LCID {PtVar($1.i, $1.v)}
  | LCURLY Patterns RCURLY {PtTuple ($1, $2)}
  | LCID Pattern {PtConstructor ($1.i, $1.v, $2)}
  | LPAREN Pattern RPAREN {$2}  

Patterns :
    Pattern {[$1]}
  | Pattern COMMA Patterns {$1 :: $3}

AppTerm :
    PathTerm
      { $1 }
  | LABEL PathTerm
      { fun ctx -> TmLabel($1, $2 ctx) }
  | UNLABEL PathTerm
      { fun ctx -> TmUnLabel($1, $2 ctx) }
  | ALIGN PathTerm
      { fun ctx -> TmAlign($1, $2 ctx) }
  | AppTerm PathTerm
      { fun ctx ->
          let e1 = $1 ctx in
          let e2 = $2 ctx in
          TmApp(tmInfo e1,e1,e2) }
  | FIX PathTerm
      { fun ctx ->
          TmFix($1, $2 ctx) }
  | BinaryOp PathTerm PathTerm
      { fun ctx -> let (info, op) = $1 in TmPrimaryOp(info, op, [$2 ctx; $3 ctx])}
  | UnaryOp PathTerm
      { fun ctx -> let (info, op) = $1 in TmPrimaryOp(info, op, [$2 ctx])}

UnaryOp:
    NOT 
      {($1, ("not", [TyBool], TyBool,
        fun inp -> match inp with
          (fi, [TmTrue _]) -> TmFalse fi
        | (fi, _) -> TmTrue fi
      ))}

BinaryOp:
    PLUS 
      {($1, ("+", [TyInt; TyInt], TyInt, 
        fun inp -> match inp with
          (fi, [TmInt (_, x); TmInt (_, y)]) -> TmInt (fi, x + y)
        | _ -> TmInt (dummyinfo, 0) 
       ))}
  | DASH
      {($1, ("-", [TyInt; TyInt], TyInt, 
        fun inp -> match inp with
          (fi, [TmInt (_, x); TmInt (_, y)]) -> TmInt (fi, x - y)
        | _ -> TmInt (dummyinfo, 0) 
       ))}
  | STAR
      {($1, ("*", [TyInt; TyInt], TyInt,
        fun inp -> match inp with
          (fi, [TmInt (_, x); TmInt (_, y)]) -> TmInt (fi, x * y)
        | _ -> TmInt (dummyinfo, 0)
        ))}
  | SLASH
      {($1, ("/", [TyInt; TyInt], TyInt,
        fun inp -> match inp with
          (fi, [TmInt (_, x); TmInt (_, y)]) -> TmInt (fi, x / y)
        | _ -> TmInt (dummyinfo, 0)
        ))}
  | EQEQ
      {($1, ("==", [TyInt; TyInt], TyBool,
        fun inp -> match inp with
          (fi, [TmInt (_, x); TmInt (_, y)]) -> if x = y then TmTrue fi else TmFalse fi
        | _ -> TmFalse dummyinfo
        ))}
  | LT
      {($1, ("<", [TyInt; TyInt], TyBool,
        fun inp -> match inp with
          (fi, [TmInt (_, x); TmInt (_, y)]) -> if x < y then TmTrue fi else TmFalse fi
        | _ -> TmFalse dummyinfo
      ))}
  | LEQ
      {($1, ("<=", [TyInt; TyInt], TyBool,
        fun inp -> match inp with
          (fi, [TmInt (_, x); TmInt (_, y)]) -> if x <= y then TmTrue fi else TmFalse fi
        | _ -> TmFalse dummyinfo
      ))}
  | GEQ
      {($1, (">=", [TyInt; TyInt], TyBool,
        fun inp -> match inp with
          (fi, [TmInt (_, x); TmInt (_, y)]) -> if x >= y then TmTrue fi else TmFalse fi
        | _ -> TmFalse dummyinfo
      ))}
  | GT
      {($1, (">", [TyInt; TyInt], TyBool,
        fun inp -> match inp with
          (fi, [TmInt (_, x); TmInt (_, y)]) -> if x > y then TmTrue fi else TmFalse fi
        | _ -> TmFalse dummyinfo
      ))}
  | AND
      {($1, ("and", [TyBool; TyBool], TyBool,
        fun inp -> match inp with
          (fi, [TmTrue _; TmTrue _]) -> TmTrue fi
        | (fi, _) -> TmFalse fi
      ))}
  | OR
      {($1, ("or", [TyBool; TyBool], TyBool,
        fun inp -> match inp with
          (fi, [TmFalse _; TmFalse _]) -> TmFalse fi
        | (fi, _) -> TmTrue fi
      ))}

TermSeq :
    Term 
      { $1 }
  | Term SEMI TermSeq 
      { fun ctx ->
          TmApp($2, TmAbs($2, "_", TyUnit, $3 (addname ctx "_")), $1 ctx) }

/* Atomic terms are ones that never require extra parentheses */
ATerm :
    LPAREN TermSeq RPAREN  
      { $2 } 
  | TRUE
      { fun ctx -> TmTrue($1) }
  | FALSE
      { fun ctx -> TmFalse($1) }
  | UNIT
      { fun ctx -> TmUnit($1) }
  | LCID 
      { fun ctx ->
          TmVar($1.i, name2index $1.i ctx $1.v, ctxlength ctx) }
  | LCURLY Fields RCURLY
      { fun ctx ->
          TmTuple($1, $2 ctx) }
  | INTV
      {fun ctx -> TmInt($1.i, $1.v)}

Fields :
    Term
      { fun ctx -> [$1 ctx] }
  | Term COMMA Fields
      { fun ctx -> ($1 ctx) :: ($3 ctx) }

TyBinder :
    /* empty */
      { fun ctx -> TyVarBind }
  | EQ Type
      { fun ctx -> TyAbbBind($2 ctx) }


/*   */
