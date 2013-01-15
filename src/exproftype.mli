open Ast
open Matching

type rule = { priority : Ast.priority; name : Ast.value_variable; sch : sch}

module Dn : sig
  type t
  exception NotFound
  exception ElabFail of string
  exception AddFail of value_variable * sch
  val empty : t
  val find : t -> typ -> rule list
  val add : t -> rule -> t
  (*val get : t -> value_variable -> sch*)
end

val exproftype : Dn.t -> typ -> Ast.expression 
