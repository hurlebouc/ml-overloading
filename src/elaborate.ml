open Ast
open Type
open Matching
open Exproftype

module SM = StringMap

(* ------------------------------------------------------------------------- *)

(* The elaborator receives a term that has been alpha-converted so that
 bindings should not shadow one another.

 It uses separate environments 
 [tvenv] to map type variables to unit,
 [vvenv] to map value variables to type schemes
 [ivenv] to keep a structure of avaivalue implicit value variables with 
 their type schemes of type [Exproftype.I.t] *)

(* We collect all environments together *)
type tvenv = unit StringMap.t
type vvenv = sch StringMap.t

type env = {
  dcenv : Wf.data_constructor_table; 
  tvenv : tvenv;
  vvenv : vvenv;
  ivenv : Dn.t;
}

(* ------------------------------------------------------------------------- *)

let rec elaborate_expr env (e : expression) : sch * expression =

  (* fonction dispatchant les variables dans les bons environnements *)

  let rec make_new_env env = function
    | [] -> env
    | (None, x, s, _)::tl -> 
        let env' = {
          dcenv = env.dcenv; 
          tvenv = env.tvenv;
          vvenv = SM.add x s (env.vvenv);
          ivenv = env.ivenv;
        } in 
          make_new_env env' tl
    | (Some p, x, s, _)::tl ->
        let rule = {
          priority = p;
          name = x;
          sch = s
        } in
        let env' = {
          dcenv = env.dcenv; 
          tvenv = env.tvenv;
          vvenv = env.vvenv;
          ivenv = Dn.add (env.ivenv) rule;
        } in 
          make_new_env env' tl      
  in

(* ---------------------------- Filtrage de motifs ------------------------- *)

  match e with
    | EVar(x)-> 
        (
          try (StringMap.find x (env.vvenv)), e with
            | Not_found -> Error.error [Error.Expr(e)] "Erreur"
            | _ -> Error.error [Error.Expr(e)] "Erreur"
        )
    | EFun(x, t2, m1)-> 
        let nenv = {
          dcenv = env.dcenv; 
          tvenv = env.tvenv;
          vvenv = SM.add x ([], ([], t2)) (env.vvenv);
          ivenv = env.ivenv;
        } in (
          match elaborate_expr nenv m1 with
            | (([],([],t1)) ,n1) -> ([],([],TArrow(t2, t1))), EFun(x, t2, n1)
            |_ -> Error.error [Error.Expr(e)] "Erreur"
        )
    | EApp(m1, m2) -> (
        match (elaborate_expr env m1), (elaborate_expr env m2) with
          | (([],([],TArrow(t2, t1))), n1), (([],([],t2')), n2) when t2=t2'->

              (* Le "=" nest pas trop restrictif car les termes sont 
               * explicitement spécifiés *)

              ([], ([], t1)), (EApp(n1, n2))
          |_, _ -> Error.error [Error.Expr(e)] "Erreur"
      )
    | EConApp(c, lt, le) ->

        let rec printlisteType = function
          | [] -> ()
          | h::t ->  
              Printf.printf "%s\n" (to_string h);
              printlisteType t;
        in
        let rec printlisteVar = function
          | [] -> ()
          | h::t ->  
              Printf.printf "%s\n" h;
              printlisteVar t;
        in
        let Wf.Scheme(tvl, tl, t) = SM.find c (env.dcenv) in
        Printf.printf "Type constructeur : %s\n" (to_string t);
        (*printlisteType tl;*)
        printlisteVar tvl;
        Error.error [Error.Expr(e)] "Constructors not implemented"













    | ELet(p, x, m1, m2) -> 
        let (s1, n1) = elaborate_expr env m1 in
        let nenv = make_new_env env [(p, x, s1, None)] in 
        let (s2, n2) = elaborate_expr nenv m2 in 
          (s2, ELet(None, x, n1, n2))
    | ELetRec(l, m2) ->
        let nenv = make_new_env env l in
        let rec elab accu = function
          | [] -> accu
          | (_, x, s1, m1)::tl -> 
              let (s1', n1) = elaborate_expr nenv m1 in
                if s1 <> s1' then Error.error [Error.Expr(e)] "Erreur" else
                  
                  (* De même ici *)
                  
                  let accu' = (None, x, s1, n1)::accu in
                    elab accu' tl
        in
        let (s2, n2) = elaborate_expr nenv m2 in 
          (s2, ELetRec(elab [] l, n2))
    | EMatch(eps, l) -> Error.error [Error.Expr(e)] "Match yet implemented"
    | EImplicit(t) -> Error.error [Error.Expr(e)] "Not yet implemented"
    | EFunI(eps, x, t, m) -> Error.error [Error.Expr(e)] "Not yet implemented"
    | ELam(a,m) -> Error.error [Error.Expr(e)] "Not yet implemented"
    | ETapp(m,t) -> Error.error [Error.Expr(e)] "Not yet implemented"

(* ------------------------------------------------------------------------- *)

(* To conclude, apply the elaborator to the empty environment *)


let elaborate_expr dcenv iml =
  let env =
    { dcenv = dcenv; 
      tvenv = StringMap.empty;
      vvenv = StringMap.empty;
      ivenv = Dn.empty;
    }  in
    Error.handle (elaborate_expr env)  iml

