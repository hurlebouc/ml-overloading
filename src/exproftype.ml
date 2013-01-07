open Settings
open Ast
open Printast
open Format
open Matching

(* the type rules *)
type rule = {
  priority : Ast.priority;
  name : Ast.value_variable;
  sch : sch
}

let print_rule pp r =
  fprintf pp "%s %s : %a" (string_of_I (Some r.priority)) r.name
    (print_sch false) r.sch

module type DN = sig
  type t
  exception NotFound
  val empty : t
  val find : t -> typ -> rule list
  val add : t -> rule -> t
  val get : t -> value_variable -> sch
end


(* The Dn structure implements a map from type to a list of rules. 
 [find m t], should return a superset of rules that could be applied
 to create a value of the corrresponding type---ideally, only rules of [m]
 whose codomain skeleton is a prefix of [t]. 

 Ideally, this should return a small superset (i.e. a good approximation)
 in time proportional to the input type, using a discimination net.  

 However, a naive implementation may just return all rules. *)


module Dn : DN = struct
  type t = {
    low : rule list;
    normal : rule list;
    high : rule list
  }

  exception NotFound

  let empty = { low = []; normal = []; high = [] }

  (* cette fonction est inutiles (ses entrées-sorties) ne me permettent pas de
   * faire grand chose d'intéressant
   *)

  let find (m : t) (t0 : typ) : rule list =
    failwith "I'm a poor lonesome function..."

  (* Il faut améliorer la fonction d'ajout en la rendant plus restrictives sur
   * les ambiguités *)

  let add (m : t) (rule : rule) : t =
    match rule.priority with
      | Low -> {
          low = rule::m.low;
          normal = m.normal;
          high = m.high;
        }
      | Normal -> {
          low = m.low;
          normal = rule::m.normal;
          high = m.high;
        }
      | High -> {
          low = m.low;
          normal = m.normal;
          high = rule::m.high;
        }

  let get (m : t) (x : Ast.value_variable) : Ast.sch =
    let rec get_aux x = function
      | [] -> raise NotFound
      | h::t -> if h.name = x then h.sch else get_aux x t
    in try get_aux x m.low with
      | _ -> try get_aux x m.normal with
          | _ -> get_aux x m.high

end

(* Cette fonction gère les appeles récursifs de la règle et vérifie que les
 * crit!ères de terminaisons sont respecté *)

let exproftype (ivenv : Dn.t) (t : typ) : expression =
  failwith "It's the hard law of the West!"
