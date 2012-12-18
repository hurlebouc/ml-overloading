open Format
open Ast

let unit = TConApp ("unit", [])

let rec size = function
  | TFvar v -> 0
  | TGvar v -> 0
  | TArrow (t1, t2) -> 1 + size t1 + size t2
  | TConApp (tc, ts) -> List.fold_left (fun n t -> n + size t) 1 ts 

(* transforms types by performing fv on fvar and gv on gvar *)
let rec lift f g = function
  | TFvar x -> f x
  | TGvar x -> g x
  | TArrow (t1, t2) -> TArrow (lift f g t1, lift f g t2)
  | TConApp (dc, ts) -> TConApp (dc, List.map (lift f g) ts)

let rec fold f g acc  = function
  | TFvar v -> f v acc 
  | TGvar v -> g v acc
  | TArrow (t1, t2) -> fold f g (fold f g acc t1) t2
  | TConApp (tc, ts) -> List.fold_left (fold f g) acc ts

let rec iter f g  = function
  | TFvar v -> f v 
  | TGvar v -> g v 
  | TArrow (t1, t2) -> iter f g t1; iter f g t2
  | TConApp (tc, ts) -> List.iter (iter f g) ts


(* Handy for fv  or gv default actions  *)
let id x = x
let skip x acc = acc
let pick x acc = x :: acc

(* collect the list of gvars in a type *)
let gvars acc t = fold skip pick acc t

(* Maps on variables. *)
module Var = struct
  type t = type_variable
  let equal p q = p = q
  module Map = Map.Make (struct
    type t = type_variable
    let compare = compare
  end)
end
type substitution = typ Var.Map.t

(* printing types *)

let print_fvar pp name = fprintf pp "%s" name
let print_gvar pp name = fprintf pp "'%s" name

let rec print0 pf pp =
  let print_typ = print pf in
  let print_typ0 = print0 pf in
  function
    | TFvar v ->
        pf pp v
    | TGvar v ->
        print_gvar pp v
    | TConApp(cstr, []) ->
        fprintf pp "%s" cstr
    | TConApp(cstr, [t1]) ->
        fprintf pp "%a %s" print_typ0 t1 cstr
    | TConApp(cstr, t1 :: tl) ->
        fprintf pp "(%a" print_typ t1;
        List.iter (fun t -> fprintf pp ", %a" print_typ t) tl;
        fprintf pp ") %s" cstr
    | t ->
        fprintf pp "@[<hov 2>(%a)@]" print_typ t

and print pf pp =
  let print_typ = print pf in
  let print_typ0 = print0 pf in
  function
    | TArrow(t1, t2) ->
        fprintf pp "@[<hov 0>%a ->@ %a@]" print_typ0 t1 print_typ  t2
    | t ->
        print_typ0 pp t

let to_string ty =
  print print_gvar str_formatter ty;
  flush_str_formatter()
