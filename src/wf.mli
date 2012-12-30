open Ast

(* [check_program p] checks that the program [p] is well-formed and returns a
 data constructor table. *)

(* A program is well-formed if every name is properly bound, every
 multiple-name-binding construct is linear, and every data
 constructor and type constructor is applied to an appropriate
 number of arguments. *)

(* A data constructor table maps every data constructor to a closed
 type scheme, where a type scheme is a triple of a vector of
 quantifiers, a vector of domain types, and a codomain type. *)

(*
 * Ici, le type du codomaine ne peut être qu'un type 
 * TConApp(name, type_variable list)
 * Les arguments de [scheme] sont
 *  - La liste des TOUTES les variables de type libres ayant servies à faire le
 *  type que construit le constructeur
 *  - La liste des types des paramètres du constructeur (décrit avec les
 *  variables libres ayant servies lors de sa définition)
 *  - le type de retour (codomaine) du cnstructeur tel qu'il a été défini (c'est
 *  à dire avec les variables de types parametrant le datatype).
 *)

type scheme =
  | Scheme of (type_variable list) * (typ list) * (typ)

type data_constructor_table =
    scheme StringMap.t

val wf_program: program -> data_constructor_table * program

