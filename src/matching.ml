open Format
open Ast
open Printast



(* Errors *)
let mismatch_sch m sch =
  Error.delay [] 
    (Printf.sprintf "Typechecking error: %s\n%s" m (sch_to_string sch))

let sch_as_typ sch = 
  match sch with
    | [], ([], t) -> t
    | _ -> mismatch_sch "type" sch

let sch_as_row sch = 
  match sch with
    | [], r -> r
    | _ -> mismatch_sch "row" sch

let sch_as_arrow sch = 
  match sch with
    | [], ([], TArrow (t1, t2)) -> t1, t2
    | _ -> mismatch_sch "arrow" sch

let sch_as_dccon sch = 
  match sch with
    | [], ([], TConApp (dc, ts)) -> dc, ts
    | _ -> mismatch_sch "constructed type" sch

(* [assert_wf_scheme (tvs, row) ] checks that all generic variables and no free
 variable of  [row] are in tvs *)
let assert_wf_scheme (tvs, (ts, t)) =
  let gv x = assert (List.mem x tvs) in
  let fv x = assert (not (List.mem x tvs)) in
    List.iter (Type.iter fv gv) ts; Type.iter fv gv t;
    true

module Var = Type.Var

(* We either substitute fvars or gvars *)
let substitute_fvars theta t =
  let fvar tv = try Var.Map.find tv theta with Not_found -> TFvar tv in
  let gvar tv = TGvar tv in
    Type.lift fvar gvar t

let substitute_gvars theta t =
  let fvar tv = TFvar tv in
  let gvar tv = try Var.Map.find tv theta with Not_found -> TGvar tv in
    Type.lift fvar gvar t

(* [close_scheme tvs row]  builds the scheme tvs, row' by making variables tvs
 generic in [row'] *)
let map_row f (ts, t) = List.map f ts, f t

let close_scheme tvs row : sch =
  let theta =
    List.fold_left (fun m tv -> Var.Map.add tv (TGvar tv) m)
      Var.Map.empty tvs in
    tvs, map_row (substitute_fvars theta) row


(*let close_scheme = failwith "Closure non implemented"*)

(* [matching sch typ] checks whether the codomain of [sch] is more general than 
 [typ], This is gven [sch] of the form [tvs, (ts, t)], it checks whether
 variables [tvs] can be instantiated so that [t] becomes equal to [typ].
 It then returns a mapping [su] from each variable of [tvs] to its instance, and
 the domain [ts] of [sch] instantiated by  [su].
 [su] is returned as an association list, not as a substitution *)



(*let matching = failwith "Matching non implemented"*)
