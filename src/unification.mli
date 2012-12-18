open Ast

(* ------------------------------------------------------------------------- *)

(* Unification variables. *)

type variable = type_variable

(* Our first-order terms are types with variables at the leaves. *)

type term = typ


(* A substitution maps variables to terms. *)

(* A unification problem is a pair of a domain (a set of variables) and
   a conjunction of term equations. Unification problems must be closed,
   that is, every variable that appears within an equation must appear
   in the domain as well. *)

type equations =
    (term * term) list

type problem =
  | Problem of variable list * equations

(* ------------------------------------------------------------------------- *)


(****
(* ------------------------------------------------------------------------- *)

(* [Build] helps construct unification problems in a pseudo-imperative style. *)

module Build (X : sig end) : sig

  (* [fresh()] creates a fresh variable and makes it part of the
     current unification problem. *)

  val fresh: unit -> variable

  (* [t1 =?= t2] adds an equation between the terms [t1] and [t2] to
     the current unification problem. *)

  val (=?=): term -> term -> unit

  (* [finished()] is used to signal that the client is done calling
     [fresh] and [=?=]. It returns the complete unification
     problem. *)

  val finished: unit -> problem

end
****)

(* ------------------------------------------------------------------------- *)

(* [mgu p] either returns the most general unifier of the unification
   problem [p], or fails by raising [Mismatch] or [OccurCheck]. *)

exception Mismatch
(* val mgu: problem -> substitution *)

val unify : typ -> typ -> bool
