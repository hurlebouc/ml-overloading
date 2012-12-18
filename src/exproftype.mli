open Ast
open Matching

type rule = { priority : Ast.priority; name : Ast.value_variable; sch : sch}

module Dn : sig
  type t
  val empty : t
  val find : t -> typ -> rule list
  val add : t -> rule -> t
end

val exproftype : Dn.t -> typ -> Ast.expression 
