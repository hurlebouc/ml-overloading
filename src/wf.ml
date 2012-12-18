open Error
open Ast
open Matching

(* ------------------------------------------------------------------------- *)

(* We check well-formedness of programs and perform alpha-conversion
   so that terms are alpha-clean: variables are renamed with some suffix
   so that there is no name shadowing. 

   We maintain a mapping from source variables to the integer use to
   suffix them.  
*)

(* Type of constructors look as type schemes, but the arguments are the 
   codomain. Constructors should always be fully applied. *)

type scheme =
  | Scheme of type_variable list * typ list * typ

type data_constructor_table =
    scheme StringMap.t

(* ------------------------------------------------------------------------- *)

(* Basic checks and error messages. *)

(* No attempt is made at providing accurate locations. *)

let ill_rhs_letrec x =
  Error.delay [] (Printf.sprintf "Recursive definition %s: %s." x
                 "right hand side should be a functions" )

let empty_lerrec x =
  Error.delay [] (Printf.sprintf "Recursive definition: %s." 
                 "should have at least one clause" )

let unbound category x =
  Error.delay [] (Printf.sprintf "Unbound %s: %s." category x)

let duplicate category x =
  Error.delay [] (Printf.sprintf "Duplicate %s: %s." category x)

let arity_check category kind c (expected : int) (actuals : 'a list) =
  let got = List.length actuals in
  if expected <> got then
    Error.delay [] begin
      Printf.sprintf
	"The %s %s expects %d %s parameter(s),\n\
	 but is here applied to %d %s argument(s)."
	category c expected kind got kind
    end

let add category x d m =
  try
    StringMap.strict_add x d m
  with StringMap.StrictAdd _ ->
    duplicate category x

let lookup category m x =
  try
    StringMap.find x m
  with Not_found ->
    unbound category x
let lookup_tv = lookup "type variable" 
let lookup_vv = lookup "value variable" 

(* ------------------------------------------------------------------------- *)

let introduce x env =
  let rec add n =
    let xn = if n = 0 then x else x ^ ("" ^ string_of_int n) in
    try let _ = StringMap.find xn env in add (succ n)
    with Not_found -> StringMap.add x xn env in
  add 0

let check_no_duplicate category xs =
  let _ = 
      List.fold_right (fun x env -> add category x () env) xs StringMap.empty
  in ()

let check_no_duplicate_tv = check_no_duplicate "type variable";;
let check_no_duplicate_vv = check_no_duplicate "value variable";;

(* Check that a type is well-formed under a map [tcenv] of type
   constructors to  arities and a set [tvenv] of type variables.  
   and returns an alpha-converted form. *)

let rec wf_typ tcenv tvenv = function
  | TFvar x ->
      TFvar (lookup_tv tvenv x)
  | TGvar x ->
      TGvar (lookup_tv tvenv x)
  | TArrow (t1, t2) ->
      TArrow (wf_typ tcenv tvenv t1, wf_typ tcenv tvenv t2)
  | TConApp (tc, ts) ->
      let expected = lookup "type constructor" tcenv tc in
      arity_check "type constructor" "type" tc expected ts;
      TConApp (tc, wf_typs tcenv tvenv ts)

and wf_typs tcenv tvenv ts =
  List.map (wf_typ tcenv tvenv) ts

and wf_sch tcenv tvenv (tvs, (ts, t)) =
  let () = check_no_duplicate_tv tvs in
  let tvenv = List.fold_right introduce tvs tvenv in
  let tvs = List.map (lookup_tv tvenv) tvs in
  let row = let f = wf_typ tcenv tvenv in List.map f ts, f t in
  close_scheme tvs row
  
(* ------------------------------------------------------------------------- *)

(* Check that a data constructor definition is well-formed. Construct a map
   [dcenv] of data constructors to their argument types. This is where we
   build the type scheme associated with each data constructor. *)

let wf_datacon_definition tcenv xs tc dcenv (DefCon (tag, ts)) =
  Error.at_location [ Definition ("constructor", tag) ]
    begin fun () -> 
      let () = check_no_duplicate_tv xs in
      let tvenv = List.fold_right introduce xs StringMap.empty in
      let xs' = List.map (lookup_tv tvenv) xs in
      let t' = TConApp (tc, List.map (fun x -> TFvar x) xs') in 
      let ts' = wf_typs tcenv tvenv ts in
      (* Turns all variables into generic variables *)
      let xs', (ts', t') = close_scheme xs' (ts', t') in
      let scheme = Scheme (xs', ts', t') in
      add "data constructor" tag scheme dcenv
    end ()

(* ------------------------------------------------------------------------- *)

(* Check that (the right-hand side of) a data type definition is
   well-formed. Construct [dcenv]. *)

let wf_datatype_definition tcenv dcenv (DefDataType (xs, tc, dcdefs)) =
  Error.at_location [ Definition ("type",  tc) ] begin fun () ->
    List.fold_left (wf_datacon_definition tcenv xs tc) dcenv dcdefs
  end ()

(* ------------------------------------------------------------------------- *)

(* Check that the data type definitions are well-formed. Construct
   [tcenv] and [dcenv]. *)

let wf_datatype_definitions defs =
  let tcenv =
    List.fold_right (fun (DefDataType (xs, tc, _)) tcenv ->
      add "type constructor" tc (List.length xs) tcenv
                    ) defs StringMap.empty
  in
  let dcenv =
    List.fold_left (wf_datatype_definition tcenv) StringMap.empty defs
  in
  tcenv, dcenv

(* ------------------------------------------------------------------------- *)

(* Check that a data constructor application is well-formed with
   respect to [dcenv] -- that is, the data constructor exists and is
   applied to an appropriate number of arguments. *)

let check_conapp dcenv dc targs vargs =
  let Scheme (xs, ts, t) = lookup "data constructor" dcenv dc in
  arity_check "data constructor" "type" dc (List.length xs) targs;
  arity_check "data constructor" "value" dc (List.length ts) vargs
  

(* ------------------------------------------------------------------------- *)

(* Check that a pattern is well-formed with respect to [dcenv]. Construct
   the pattern's domain. *)

let wf_pat_var domain x =
  introduce x domain 

let wf_pat tcenv dcenv tvenv vvenv (PConApp (dc, ts, xs)) =
  check_conapp dcenv dc ts xs;
  let () = check_no_duplicate_vv xs in
  let ts' = wf_typs tcenv tvenv ts in
  let vvenv = List.fold_left wf_pat_var vvenv xs in
  let xs' = List.map (lookup_vv vvenv) xs in
  vvenv, PConApp (dc, ts', xs')

(* ------------------------------------------------------------------------- *)

(* Check expressions and branches. *)

let rec wf_branch tcenv dcenv tvenv vvenv (Branch (p, e)) =
  let vvenv, p' = wf_pat tcenv dcenv tvenv vvenv p in
  Branch (p', wf_exp tcenv dcenv tvenv vvenv e)

and wf_exp_reloc x tcenv dcenv tvenv venv =
  Error.at_location [ Error.Binding x ] (wf_exp tcenv dcenv tvenv venv)
    
and wf_exp tcenv dcenv tvenv (vvenv : string StringMap.t) = function
  | EVar x ->
      EVar (lookup_vv vvenv x)
  | EFun (x, t, e) ->
      let vvenv = introduce x vvenv in
      let x' = lookup_vv vvenv x in
      EFun (x', wf_typ tcenv tvenv t, wf_exp tcenv dcenv tvenv vvenv e)
  | EFunI (q, x, t, e)->
      let vvenv = introduce x vvenv in
      let x' = lookup_vv vvenv x in
      EFunI (q, x', wf_typ tcenv tvenv t, wf_exp tcenv dcenv tvenv vvenv e)

  | EApp (e1, e2) ->
      let wf = wf_exp tcenv dcenv tvenv vvenv in EApp (wf e1, wf e2)
  | EConApp (dc, ts, es) ->
      let ts' = List.map (wf_typ tcenv tvenv) ts in
      check_conapp dcenv dc ts es;
      let es' = check_exps tcenv dcenv tvenv vvenv es in
      EConApp (dc, ts', es') 
  | ELet (b, x, e1, e2) ->
      let vvenv = introduce x vvenv in
      let e1' = wf_exp_reloc x tcenv dcenv tvenv vvenv e1 in 
      let e2' = wf_exp tcenv dcenv tvenv vvenv e2 in 
      ELet (b, lookup_vv vvenv x, e1', e2')

  | ELetRec ([], e) ->
      wf_exp tcenv dcenv tvenv vvenv e

  | ELetRec (bs, e) ->
      let vvenv =
        List.fold_right
          (fun (ui, xi, ti, ei) vvenv ->
            introduce xi vvenv)
          bs vvenv
      in
      let  bs' =
        List.map
          begin function (ui, xi, si, ei) ->
            Error.at_location [Binding xi] begin fun () -> 
              (* as wf_exp but check that arg is a function *)
              let rec wf_rhs tvenv = function
                | ELam (vs, e) ->
                    let tvenv = List.fold_right introduce vs tvenv  in
                    ELam (List.map (lookup_tv tvenv) vs, wf_rhs tvenv e)
                | EFun (_, _, _) 
                | EFunI (_, _, _, _) as e ->
                    wf_exp tcenv dcenv tvenv vvenv e
                | _ -> ill_rhs_letrec xi 
              in
              let xi' = lookup_vv vvenv xi in
              let si' = wf_sch tcenv tvenv si in
              let ei' = wf_rhs tvenv ei in
              (ui, xi', si', ei')
            end ()
          end
          bs in 
      ELetRec (bs', wf_exp tcenv dcenv tvenv vvenv e)

  | EMatch (e, bs) ->
      EMatch
        (wf_exp tcenv dcenv tvenv vvenv e,
         List.map (wf_branch tcenv dcenv tvenv vvenv) bs)
  | EImplicit t ->
      EImplicit (wf_typ tcenv tvenv t)
  | ELam (vs, e) ->
      let tvenv = List.fold_right introduce vs tvenv in
      ELam (List.map (lookup_tv tvenv) vs, wf_exp tcenv dcenv tvenv vvenv e)
  | ETapp (e, ts) ->
      ETapp (wf_exp tcenv dcenv tvenv vvenv e,
             List.map (wf_typ tcenv tvenv) ts)

and check_exps tcenv dcenv tvenv vvenv es =
  List.map (wf_exp tcenv dcenv tvenv vvenv) es

(* ------------------------------------------------------------------------- *)

(* Check that a program is well-formed and return the data constructor
   table. *)

let wf_program (Program (defs, e)) =
  let tcenv, dcenv =
    Error.handle wf_datatype_definitions defs in
  let e' =
    Error.handle (wf_exp tcenv dcenv StringMap.empty StringMap.empty) e in
  dcenv, (Program (defs, e'))

