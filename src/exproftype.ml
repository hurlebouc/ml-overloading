open Settings
open Ast
open Printast
open Format
open Matching
open Type

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
  exception ElabFail of string
  exception AddFail of value_variable * sch
  val empty : t
  val find : t -> typ -> rule list
  val add : t -> rule -> t
  (*val get : t -> value_variable -> sch*)
end


(* The Dn structure implements a map from type to a list of rules. 
 [find m t], should return a superset of rules that could be applied
 to create a value of the corrresponding type---ideally, only rules of [m]
 whose codomain skeleton is a prefix of [t]. 

 Ideally, this should return a small superset (i.e. a good approximation)
 in time proportional to the input type, using a discimination net.  

 However, a naive implementation may just return all rules. *)


let string_of_rule r =
  let prio = match r.priority with
    | Low -> "low"
    | Normal -> "normal"
    | High -> "high"
  in r. name ^ " : " ^ prio ^ ", [" ^ (Printast.sch_to_string r.sch) ^ "]";;

let print_rule_list = List.iter (fun x -> printf "%s\n" (string_of_rule x));;

module Dn : DN = struct
  type t = {
    low : rule list;
    normal : rule list;
    high : rule list
  }

  exception NotFound
  exception ElabFail of string
  exception AddFail of value_variable * sch

  let empty = { low = []; normal = []; high = [] }

  (* Cette fonction se contente de tout renvoyer dans le bon ordre. *)

  let find (m : t) (t0 : typ) : rule list =
    (m.high) @ (m.normal) @ (List.rev m.low)

  let add (m : t) (rule : rule) : t =
    match rule.priority with
      | Low -> {
          low = rule::m.low;
          normal = m.normal;
          high = m.high;
        }
      | Normal ->
          (*Printf.printf "%s\n" (string_of_rule rule);*)
          let (_, (_, t)) = rule.sch in
          let p r = 
            let (_, (_, t')) = r.sch in
              (r.name <> rule.name) && (Unification.unify t' t)
          in
            (
              try 
                let rule = List.find p (m.normal) in
                  raise (AddFail(rule.name, rule.sch))
              with
                | Not_found -> {
                    low = m.low;
                    normal = rule::m.normal;
                    high = m.high;
                  }
            )
      | High -> {
          low = m.low;
          normal = m.normal;
          high = rule::m.high;
        }

  (*let get (m : t) (x : Ast.value_variable) : Ast.sch =
    let rec get_aux x = function
      | [] -> raise NotFound
      | h::t -> if h.name = x then h.sch else get_aux x t
    in try get_aux x m.low with
      | _ -> try get_aux x m.normal with
          | _ -> get_aux x m.high*)

end


(* renvoie true lorsque l'ajout de la variable et du type est légal *)
let test_bf path ((x0 : value_variable), (t0 : typ)) : bool =
  let p (x, t) = (t=t0) || (x=x0 && (size t0 > size t)) in
    not (List.exists p path);;
  (*true;;*)

(* Cette fonction gère les appels récursifs de la règle et vérifie que les
 * critères de terminaisons sont respectés *)

let exproftype (ivenv : Dn.t) (t0 : typ) : expression =

  let listchoice = (Dn.find ivenv t0) in
  
  (*printf "elaboration de %s\n" (to_string t0);
  print_rule_list listchoice;*)

  let reorder subst s = 
    let (tv, _) = s in
    let p x (x', y) = x'=x in
    let find_couple x = List.find (p x) subst in
    let find_type x = snd (find_couple x) in
      List.map find_type tv
  in

  let rec apply_mult head args = match args with
    | [] -> head
    | h::t -> apply_mult (EApp(head, h)) t 
  in

  let rec aux (path : (value_variable * typ) list) t0 = function
    | [] -> None
    | rule::tail -> 
        let x, s = rule.name, rule.sch in
          match matching s t0 with
            | Some(subst, new_row) when test_bf path (x, t0) -> 
                let rec dispatch_types accu = function
                  | [] -> Some (List.rev accu)
                  | t'::others -> 
                      (*match aux ((x, t0)::path) t' tail with*)
                      match aux ((x, t0)::path) t' listchoice with 
                        (* On devrait normalement remettre à jour listchoice
                        * mais comme ici, tout la liste est renvoyée, ce n'est
                        * pas la peine *)
                        | None -> None
                        | Some n -> dispatch_types (n::accu) others
                in (
                  match dispatch_types [] new_row with
                    | None -> aux path t0 tail
                    | Some ln -> Some ( apply_mult (ETapp(EVar(x), reorder subst s)) ln)
                )
            | _ -> aux path t0 tail
  in
   match aux [] t0 listchoice with
     | Some n -> n
     | None -> raise 
                 (Dn.ElabFail 
                    ("L'élaboration du type [" ^ (to_string t0) ^ "] a échouée"))
