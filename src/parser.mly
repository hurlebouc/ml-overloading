(* In this simple language, we attempt to stick mostly to ocaml syntax.
   There are a few departures, though: we use MATCH ... WITH ... END
   instead of MATCH ... WITH; tuples must be explicitly parenthesized;
   and the token ";;" is used to announce the program body. *)

%{

open Ast

let etapp e1 ts =
  match e1 with
  | EConApp (tag, ts', []) -> 
      EConApp (tag, ts @ ts', [])
  | _ ->
      ETapp (e1, ts)

let eapp e1 e2 =
  match e1 with
  | EConApp (tag, ts, args) -> 
      EConApp (tag, ts, args @ [e2])
  | _ ->  EApp (e1, e2)

let emapp e el =
  match e with
  | EConApp (tag, ts, args) -> 
      EConApp (tag, ts, args @ el)
  | _ ->
      List.fold_left (fun e ei ->  EApp (e, ei))  e el

%}

%token <string> IDENTIFIER CONSTRUCTOR
%token LBRACKET LPAR COMMA RPAR RBRACKET
%token TYPE DEFEQ BAR
%token QUOTE ARROW DOUBLEARROW STAR QUESTIONMARK LANGLE RANGLE
%token LET REC AND FUN LAM MATCH WITH BEGIN END OF IN DOT COLON 
%token SEMISEMI EOF
%start <Ast.program> program

(* In ocaml syntax, constructor application (EConApp) and function
   application (EApp) have the same syntax, which makes the grammar
   ambiguous. We work around this issue by giving appropriate priority
   declarations. Here, we must prefer shifting LPAR, CONSTRUCTOR, and
   IDENTIFIER over reducing the production expression0 ->
   CONSTRUCTOR. *)

%nonassoc lone_constructor 
%nonassoc noarg_constructor
%nonassoc LPAR LBRACKET IDENTIFIER CONSTRUCTOR

%%

(* ------------------------------------------------------------------------- *)

(* Parenthesized tuples. *)

parenthesized_tuple(X):
| LPAR xs = separated_list(COMMA, X) RPAR
    { xs }

(* ------------------------------------------------------------------------- *)

(* This parameterized non-terminal symbol recognizes separated lists of at
   least two elements -- which means that at least one separator is
   explicit. The definition is inlined, so as to make the first separator
   visible at the instantiation site. *)

%inline separated_list_of_two_or_more(separator, X):
| head = X separator tail = separated_nonempty_list(separator, X)
    { head :: tail }

separated_list_of_one_or_more(separator, X):
| head = X 
    { [ head ] }
| head = X separator tail = separated_list_of_one_or_more(separator, X)
    { head :: tail }

(* ------------------------------------------------------------------------- *)

(* In ocaml syntax, BAR can be used either as a delimiter or as an opening
   symbol. That is, the first BAR is optional. Here is a way of saying so. *)

bar(X):
| xs = preceded(BAR, X)+
| xs = separated_nonempty_list(BAR, X)
    { xs }

(* ------------------------------------------------------------------------- *)

(* In ocaml syntax, the parameters of a data constructor or type
   constructor can be absent, a single parameter (without
   parentheses), or a tuple of parameters (with parentheses and
   commas). *)


parameters(X):
| (* epsilon *)
    { [] }
| x = X
    { [ x ] }
| LPAR xs = separated_list_of_two_or_more(COMMA, X) RPAR
    { xs }

(* ------------------------------------------------------------------------- *)

(* Types. *)

type_variable:
| QUOTE x = IDENTIFIER
    { x }

typ0:
| tv = type_variable
    { TFvar tv }
| ts = parameters(typ0) t = IDENTIFIER
    { TConApp (t, ts) }
| LPAR t = typ RPAR
    { t }

typ:
| t = typ0
    { t }
| t1 = typ0 ARROW t2 = typ
    { TArrow (t1, t2) }

typs:
| ts = separated_nonempty_list(COMMA, typ)
    { ts }

row:
| t = typ
    { [], t }
| t = typ DOUBLEARROW r = row
    { let (ts, t0) = r in t::ts, t0 }

sch: 
| r = row
    { [], r }
| v = type_variable DOT s = sch
    { let (vs, r) = s in v::vs, r }

(* ------------------------------------------------------------------------- *)

(* Patterns. *)

pattern:
| tag = CONSTRUCTOR xs = parameters(IDENTIFIER)
    { PConApp (tag, [], xs) }
| tag = CONSTRUCTOR LBRACKET ts = separated_list(COMMA, typ) RBRACKET
    xs = parameters(IDENTIFIER)
    { PConApp (tag, ts, xs) }

(* ------------------------------------------------------------------------- *)

(* Identifier. *)

policy: 
|
    { Normal }
| LANGLE
    { Low }
| RANGLE
    { High }

implicit:
| 
    { None }
| QUESTIONMARK q = policy
    { Some q  }    
    

(* ------------------------------------------------------------------------- *)

(* Expressions. *)

expression0:
| x = IDENTIFIER
    { EVar x }
| tag = CONSTRUCTOR %prec lone_constructor
    { EConApp (tag, [], [])  }
| LPAR QUESTIONMARK COLON t = typ RPAR
    { EImplicit (t) }
| LPAR e = expression RPAR
   { e }

expression1:
| e = expression0
    { e }
| e1 = expression1 e2 = expression0
    { EApp (e1, e2) }
| e = expression1 LBRACKET ts = typs RBRACKET
    { ETapp (e, ts) }
| tag = CONSTRUCTOR e = expression0
    { EConApp (tag, [], [e]) }
| tag = CONSTRUCTOR 
    LPAR es = separated_list_of_two_or_more(COMMA, expression) RPAR 
    { EConApp (tag, [], es) }
| tag = CONSTRUCTOR
    LBRACKET ts = typs RBRACKET 
    %prec noarg_constructor
    { EConApp (tag, ts, []) }
| tag = CONSTRUCTOR
    LBRACKET ts = typs RBRACKET
    e = expression0
    { EConApp (tag, ts, [e]) }
| tag = CONSTRUCTOR
    LBRACKET ts = typs RBRACKET 
    LPAR es = separated_list_of_two_or_more(COMMA, expression) RPAR 
    { EConApp (tag, ts, es) }

expression:
| e = expression1
    { e }
| FUN LPAR x = IDENTIFIER COLON t = typ RPAR ARROW e = expression
    { EFun (x, t, e) }
| BEGIN MATCH e = expression WITH bs = bar(branch) END
    { EMatch (e, bs) }
| LET u = implicit x = IDENTIFIER DEFEQ e1 = expression IN e2 = expression
    { ELet (u, x, e1, e2) }
| LET p = pattern DEFEQ e1 = expression IN e2 = expression
    { EMatch (e1, [ Branch (p, e2) ]) }
| LET REC ds = separated_nonempty_list (AND, let_rec_definition)
    IN e2 = expression
    { ELetRec (ds, e2) }
| FUN LPAR QUESTIONMARK q = policy x = IDENTIFIER COLON t = typ RPAR 
    DOUBLEARROW e = expression
    { EFunI (q, x, t, e) }
| LAM vs = list(type_variable) DOT e = expression
    { ELam (vs, e) }

let_rec_definition:
| u = implicit x = IDENTIFIER COLON s = sch DEFEQ e = expression
    { (u, x, s, e) }

branch:
| p = pattern ARROW e = expression
    { Branch (p, e) }

(* ------------------------------------------------------------------------- *)

(* Data type definitions. *)

constructor_parameters:
| (* epsilon *)
    { [] }
| OF ts = separated_nonempty_list(STAR, typ)
    { ts }

data_constructor_definition:
| tag = CONSTRUCTOR ts = constructor_parameters
    { DefCon (tag, ts) }

data_type_definition:
| TYPE xs = parameters(type_variable) x = IDENTIFIER
  DEFEQ cs = bar(data_constructor_definition)
    { DefDataType (xs, x, cs) }

(* ------------------------------------------------------------------------- *)

(* Programs. *)

program:
| defs = data_type_definition*
  SEMISEMI e = expression EOF
    { Program (defs, e) }

