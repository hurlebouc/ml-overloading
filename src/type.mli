(* Basic operations on types. *)

open Format
open Ast

val unit : typ

val size : typ -> int
val lift : (type_variable -> typ) -> (type_variable -> typ) -> typ -> typ
val fold : (type_variable -> 'a -> 'a) -> (type_variable -> 'a -> 'a) ->
  'a -> typ -> 'a
val iter : (type_variable -> unit) -> (type_variable -> unit) -> typ -> unit

val id : 'a -> 'a
val skip : 'a -> 'b -> 'b
val pick : 'a -> 'a list -> 'a list

(* [gvars accu t] adds to [accu] all gvars of [t] *)
val gvars : type_variable list  -> typ -> type_variable list 

(* Maps on variables. *)
module Var : sig
  type t =  type_variable
  val equal : t -> t -> bool
  module Map : Map.S with type key = type_variable
end
type substitution = typ Var.Map.t

(* If [theta] is a substitution of types for type variables, then
   [lift theta] is a type homomorphism. *)

(* This turns a type variable printer into a type printer. *)

val print: (formatter -> type_variable -> unit) -> formatter -> typ -> unit

val print0: (formatter -> type_variable -> unit) -> formatter -> typ -> unit

val print_fvar : formatter -> type_variable -> unit
val print_gvar : formatter -> type_variable -> unit

val to_string : typ -> string
