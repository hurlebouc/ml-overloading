open Format
open Ast

(* This series of functions requires sch of the form (tvs, (ts, t)) to be of
   a particular form decompose it accordingly and returns the expected form
   *)

(** sch should be a row, i.e. tvs should be empty *)
val sch_as_row : sch -> (typ list * typ)
(** sch should be a typ, i.e. ts should moreover be empty *)
val sch_as_typ : sch -> typ
(** sch should be an arrow type, i.e. should more over be an arrow *)
val sch_as_arrow : sch -> typ * typ
(** sch should be a constructed type *)
val sch_as_dccon : sch ->  type_constructor * typ list

(* [close_scheme tvs row]  builds the scheme tvs, row' by turning
   variables tvs into generic variables [row'] *)
val close_scheme : type_variable list -> row -> sch




(* [matching sch typ] checks whether the codomain of [sch] is more general
   than [typ], This is gven [sch] of the form [tvs, (ts, t)], it checks
   whether variables [tvs] can be instantiated so that [t] becomes equal to
   [typ].  It then returns a mapping [su] from each variable of [tvs] to its
   instance, and the domain [ts] of [sch] instantiated by [su].  [su] is
   returned as an association list, not as a substitution *)

(*val matching : sch -> typ -> ((type_variable * typ) list * typ list) option*)
