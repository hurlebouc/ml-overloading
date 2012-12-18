open Settings
open Ast
open Printast
open Format
open Matching

(* the type rules *)
type rule = { priority : Ast.priority; name : Ast.value_variable; sch : sch}
let print_rule pp r =
  fprintf pp "%s %s : %a" (string_of_I (Some r.priority)) r.name
    (print_sch false) r.sch

module type DN = sig
  type t
  val empty : t
  val find : t -> typ -> rule list
  val add : t -> rule -> t
end


(* The Dn structure implements a map from type to a list of rules. 
   [find m t], should return a superset of rules that could be applied
   to create a value of the corrresponding type---ideally, only rules of [m]
   whose codomain skeleton is a prefix of [t]. 

   Ideally, this should return a small superset (i.e. a good approximation)
   in time proportional to the input type, using a discimination net.  

   However, a naive implementation may just return all rules. *)

module Dn : DN = struct
  type t = { low : rule list; normal : rule list; high : rule list}
  let empty = { low = []; normal = []; high = [] }

          
  let find m t =
       failwith "Not implemented"


  let add m rule =
       failwith "Not implemented"

end


let exproftype ivenv t =
   failwith "Not implemented yet"
