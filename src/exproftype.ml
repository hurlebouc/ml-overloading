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

  type ('a, 'b) either =
    | Left of 'a
    | Right of 'b

  type dn = 
      (* ((rule * int) list option) * ((rule * int) list option) * (dn option)*)
    | ConsDN of 
        (* liste des listes des règles sous la même variables *)
        ((type_variable * (rule * int) list) list)
        (* liste des règles sous une variable liée (on ne fais pas 
         * la différence entre les variables liée : c'est une sur-approximation)*)
        * ((rule * int) list)
        (* indique si une flèche est présente *)
        * (dn*dn) option
        (* liste des différents constructeurs présents à la racine *)
        * (type_constructor * (dn list, (rule * int) list) either) list


  type t = {
    low     : dn * int (*rule list*);
    normal  : dn * int (*rule list*);
    high    : dn * int (*rule list*)
  }

  exception NotFound
  exception ElabFail of string
  exception AddFail of value_variable * sch

  let empty = {
    low = ConsDN([], [], None,  []), 0;
    normal = ConsDN([], [], None, []), 0;
    high = ConsDN([], [], None, []), 0
  }


  (* ce type est juste utlisé pour pouvoir modéliser l'univers entier des types
   * (pratique pour les cas d'initialisation des inductions)*)
  type 'a set = 
    | Infty
    | Set of 'a list

  let rec intercection s1 s2 = match s1, s2 with
    | Infty, _ -> s2
    | _, Infty -> s1
    | Set [], _ -> Set []
    | Set (hd::tl), Set(l2) -> 
        if (List.exists (fun x -> x = hd) l2) 
        then match intercection (Set(tl)) (Set(l2)) with
          | Set(l) -> Set (hd::l)
          | _ -> assert false
            else intercection (Set(tl)) (Set(l2))

  let find (m : t) (t0 : typ) : rule list =

    (* Trouve toutes règles dont le codomaine peut s'instancié t0
     * n'établie pas encore d'ordre sur les règles *)
    let rec filtre (dn : dn) (t0 : typ) : (rule*int) list =
      let ConsDN(lfvars, lgvars, arrow, lcons) = dn in
      let res =  match t0 with
        | TFvar tv ->
            let _, liste = (* on récupère l'ensemble des règles associés à la
                            * la variable tv*)
              try List.find (fun (tv', lrule) -> tv = tv') lfvars
              with
                |Not_found -> "", []
            in lgvars @ liste (* union *)
        | TGvar _ -> assert false (* cf doc *)
        | TArrow(t1,t2) -> (
            match arrow with
              | None -> lgvars
              | Some (dn1, dn2) ->
                  let l1 = filtre dn1 t1 in
                  let l2 = filtre dn2 t2 in
                    match intercection (Set(l1)) (Set(l2)) with
                      | Set l -> l @ lgvars
                      | _ -> assert false
          )
        | TConApp(tc, ltype) -> 
            try
              let _, succ = List.find (fun (tc', ldn) -> tc' = tc) lcons 
              in match succ with
                | Left ldn -> (
                    match List.fold_left2 
                            (fun accu dn t -> intercection (Set(filtre dn t)) accu) 
                            Infty ldn ltype with
                      | Infty -> assert false
                      | Set l -> l @ lgvars (* dans le cas où on a fait des 
                                             appels recursifs dans les types de 
                                             specification du constructeur de type *)
                  )
                | Right lr -> lr @ lgvars (* dans le cas où le constructeur de 
                                           type n'admet pas de spécification *)
            with
              |Not_found ->
                  lgvars (* dans le cas où aucun constructeur semblable
                          n'est déjà dans l'environnement *)
      in
         res
    in

    (* Maintenant on tri les listes en fifo ou lifo *)
    let comp = fun (r, n) (r', n') -> n - n' in
    let comp_inv = fun (r, n) (r', n') -> n' - n in
    let proj = fun (r, n) -> r in
    let high = List.map proj (List.sort comp_inv (filtre (proj m.high) t0)) in
    let normal = List.map proj (List.sort comp (filtre (proj m.normal) t0)) in
    let low = List.map proj (List.sort comp (filtre (proj m.low) t0)) in
      high @ normal @ low

  (* Cette fonction cherche dans le les règles répondant au prédicat p *)
  let rec find_exact (p : rule -> bool) (dn : dn) : rule list = 
    let ConsDN(lfvars, lgvars, arrow, lcons) = dn in
    let p' = fun (rule, n) -> p rule in

    (* recherche dans les variables génériques *)
    let res = List.filter p' lgvars in

    (* recherche dans les variables libres*)
    let res = List.fold_left 
                (fun accu (tv, lr) -> (List.filter p' lr) @ accu) res lfvars in

    (* projection de la liste sur sa première composante (les règles) *)
    let proj lRuleInt = List.map (fun (rule, n) -> rule) lRuleInt in
    let res = proj res in

    let find_exact_list p = fun ldn -> 
      List.fold_left (fun accu dn -> (find_exact p dn) @ accu) [] ldn in

    (* recherche dans les constructeurs *)
    let f = fun accu (tc, succ) ->
      match succ with
        | Left ldn -> (find_exact_list p ldn) @ accu
        | Right lr -> proj ((List.filter p' lr)) @ accu
    in

    (* recherche dans les flèches *)
    let res = List.fold_left f res lcons in
      match arrow with
        | None -> res
        | Some(dn1, dn2) -> (find_exact_list p [dn1; dn2]) @ res


  let add (m : t) (rule : rule) : t =

    (* ajoute dans un dn une règle avec le numéro n *)
    let addInDN (dn : dn) (rule : rule) (n : int) : dn =

      (* permet de s'enfoncer simultanément dans dn et le codomaine de rule sans
       * toucher à rule*)
      let rec aux (dn : dn) (t : typ) : dn = 
        let ConsDN(lfvars, lgvars, arrow, lcons) = dn in
          match t with
            | TFvar(tv) -> 
                let rec add_to_list liste tv = match liste with
                  | [] -> [(tv, [(rule, n)])] (*si pas encore de variable libre
                                                tv dans dn*)
                  | (tv', lr)::tl -> if tv = tv' 
                    then (tv', (rule, n)::lr)::tl
                    else (tv', lr)::(add_to_list tl tv)
                in
                let lfvars = add_to_list lfvars tv in
                  ConsDN(lfvars, lgvars, arrow, lcons)
            | TGvar(tv) -> let lgvars = (rule, n) :: lgvars in
                ConsDN(lfvars, lgvars, arrow, lcons)
            | TArrow(t1, t2) -> (
                match arrow with
                  | None -> (* si pas encore de flèche dans dn *) 
                      let arrow = Some (
                        aux (ConsDN([], [], None, [])) t1
                        , aux (ConsDN([], [], None, [])) t2) in
                        ConsDN(lfvars, lgvars, arrow, lcons)
                  | Some (dn1, dn2) -> 
                      let arrow = Some(aux dn1 t1, aux dn2 t2) in
                        ConsDN(lfvars, lgvars, arrow, lcons)
              )
            | TConApp(tc, ltype) ->
                let rec add_to_list lcons tc = match lcons with
                  | [] -> (
                      match ltype with
                        | [] -> [(tc, Right([(rule, n)]))]
                        | _ -> let ldn = List.map 
                                  (fun ty -> aux (ConsDN([], [], None, [])) ty)
                                  ltype 
                      in 
                        [(tc, Left (ldn))]
                    )
                  | (tc', succ)::tl -> if tc = tc' 
                    then match succ with
                      | Left ldn -> (tc', Left(List.map2 (fun dn ty -> aux dn ty) ldn ltype))::tl
                      | Right lr -> (tc', Right((rule, n)::lr))::tl
                    else (tc', succ)::(add_to_list tl tc)
                in
                let lcons = add_to_list lcons tc in
                  ConsDN(lfvars, lgvars, arrow, lcons)
      in
      let (_, (_, ty)) = rule.sch in
        aux dn ty
    in

    match rule.priority with
      | Low -> {
          low = (addInDN (fst m.low) rule (snd m.low), (snd m.low) + 1) (*rule::m.low*);
          normal = m.normal;
          high = m.high;
        }
      | Normal ->
          let (_, (_, t)) = rule.sch in

          (* définition du prédicat de conflit entre deux règles *)
          let p = fun r ->
            let (_, (_, t')) = r.sch in
              (r.name <> rule.name) && (Unification.unify t' t)
          in
            (
              match find_exact p (fst m.normal) with
                | [] -> {
                    low = m.low;
                    normal = (addInDN (fst m.normal) rule (snd m.normal), (snd m.normal) + 1);
                    high = m.high;
                  }
                | rule::_ -> raise (AddFail(rule.name, rule.sch))
            )
      | High -> {
          low = m.low;
          normal = m.normal;
          high = (addInDN (fst m.high) rule (snd m.high), (snd m.high) + 1);
        }
end


(* renvoie true lorsque l'ajout de la variable et du type est légal (les deux
* critères de la page 5)*)
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

    (* la fonction auxilière d'élaboriation demande comme premier argument le
     * chemin à effectuer pour atteindre le terme élaboré (afin de vérifier le
     * critère de terminaison) *)
  let rec aux (path : (value_variable * typ) list) t0 = function
    | [] -> None
    | rule::tail -> 
        let x, s = rule.name, rule.sch in
          match matching s t0 with

              (* lorsque le codomaine du schema est élaborable, on élabore les
               * types du domaine*)
            | Some(subst, new_row) when test_bf path (x, t0) ->
                (* effectue les constructions successives des types du domaine *)
                let rec dispatch_types accu = function
                  | [] -> Some (List.rev accu) (* domaine vide *)
                  | t'::others -> 
                      match aux ((x, t0)::path) t' (Dn.find ivenv t') with
                        | None -> None
                        | Some n -> dispatch_types (n::accu) others
                in (
                  match dispatch_types [] new_row with
                    | None -> aux path t0 tail (* l'élaboration d'un des types
                                                du domaine a échouée*)
                    | Some ln -> Some ( apply_mult (ETapp(EVar(x), reorder subst s)) ln)
                )
            | _ -> aux path t0 tail
  in
   match aux [] t0 listchoice with
     | Some n -> n
     | None -> raise 
                 (Dn.ElabFail 
                    ("L'élaboration du type [" ^ (to_string t0) ^ "] a échouée"))
