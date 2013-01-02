(* This file defines the abstract syntax of our small programming language. *)

(* ------------------------------------------------------------------------- *)

(* We begin with four kinds of identifiers: value variables, type variables,
 data constructors, and type constructors. All of them are represented as
 strings. *)

(* Value variables. *)

type value_variable =
   string

(* Type variables. *)

type type_variable =
    string

(* Data constructors. *)

type data_constructor =
    string

(* Algebraic data type constructors. *)

type type_constructor =
    string

(* ------------------------------------------------------------------------- *)

(* Types. *)

(* Types can appear within algebraic data type definitions and within
 * expressions. *)

(* A type can be: 
 * a type variable
 * a function type
 * an application of a type constructor to an appropriate number of type 
 arguments. *)

(* Note: [TArrow] is really superfluous, since it is a particular case
 of [TConApp]. We keep it, for no good reason.  

 We use two constructors for variables to distinguish in type schemes
 between free variables that cannot and generic variables that may be
 instantiated.  The difference is not made during parsing, but establish
 during well-formedness checking.  *)

(* ATTENTION : le test de bonne formation ne modifie PAS les annotations de
 * types : ce qui fait que dans le terme [Lam b fun x : b -> x], le parser donne
 * la valeur TFvar(b) au type de x dans fun. Cela peut poser des problèmes car,
 * si cette expression est annotée (lors de son utilisation dans un let par
 * exemple, le parser donnera la valeur ['b ((TGvar(b))->(TGvar(b))] à
 * l'annotation alors que lors de la reconstruction du type de l'expression dans
 * l'élaboration, le type trouvé sera ['b ((TFvar(b))->(TFvar(b))]. À cela il y a
 * plusieurs solution
 *   - lors des tests d'égalité de types, on fera attention à égaliser TGvar et
 *   TFvar
 *   - lors de la mise en environnement de variables de termes, on vérifiera si
 *   le type (obtenu par annotation) contient des variables de type liées, et
 *   dans ce cas, on modifiera le type de l'annotation en conséquence 
 *)

type typ =
  | TFvar of type_variable                  (* free variable *)
  | TGvar of type_variable                  (* bound variable *)
  | TArrow of typ * typ                     (* function type t1 -> t2 *)
  | TConApp of type_constructor * typ list  (* (t1, ..., tn) typeconstr *)

(* A row is of the form t1 => .. tp => t.
 A row is not first class and can only appear at the toplevel of a type *)

type row =
   typ list * typ

(* A type scheme is a row in which some variables are polymorphic, i.e.
 bound. *)

type sch =
    type_variable list * row

(* The order of definition is important as it defines the ordering between
 policies *)

type priority =
  | Low    (* < *)
  | Normal  (* Nothing *)
  | High   (* > *)
;;

(* ------------------------------------------------------------------------- *)

(* Case analysis patterns. *)

type pattern =
  | PConApp of data_constructor * typ list * value_variable list

(* Case analysis branches. *)

type branch =
  | Branch of pattern * expression

(* ------------------------------------------------------------------------- *)

(* Expressions. *)

(* An expression can be: 
 *    a value variable
 *    an abstraction (that is, an anonymous function)
 *    a recursive abstraction
 *    a function application
 *    a tuple (of arbitrary arity)
 *    an application of a data constructor to a single argument
 *    a case analysis
 *    an introduction of an existentially quantified type variable
 *    a type annotation
 *)

and expression =
  | EVar of value_variable
  | EFun of value_variable * typ * expression                
  (*  fun x : t -> M *)
  | EApp of expression * expression                          
  (* M1 M2 *)
  | EConApp of data_constructor * typ list * expression list 
  (* Cstr t1 .. tp (a1,...,aN) *)
  | ELet of priority option * value_variable * expression * expression
  (* let e0 x0 = M0 in M *)
  | ELetRec of
      (priority option * value_variable * sch * expression) list * expression 
  (* let rec e1 x1 : s1 =  M1 and ... en xn = Mn in M  *)
  | EMatch of expression * branch list 
  (* match M with ... *)
  | EImplicit of typ
  (* ( ? :  typ ) *)
  | EFunI of priority * value_variable * typ * expression                
  (*  fun ?q x : t => M *)
  | ELam of type_variable list * expression 
  (*  Lam 'a . M) *)
  | ETapp of expression * typ list
(*  M [t]  *)

(* ------------------------------------------------------------------------- *)

(* A data constructor definition consists of the data constructor's name,
 together with the types of its arguments. *)

type data_constructor_definition =
  | DefCon of data_constructor * typ list

(* A data type definition consists of a list of formal type
 parameters, the type constructor's name, and a list of data
 constructor definitions. *)

type data_type_definition =
  | DefDataType of
      type_variable list * type_constructor * data_constructor_definition list

(* A complete program consists of a list of data type definitions and
 an expression. *)

type program =
  | Program of data_type_definition list * expression

