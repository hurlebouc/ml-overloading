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
  match e with
    | EVar(x)-> 
        (
          try (StringMap.find x (env.vvenv)), e with
            | Not_found -> Error.error [Error.Expr(e)] "Erreur"
            | _ -> Error.error [Error.Expr(e)] "Erreur"
        )
    | EFun(x, t2, m1)-> 
        let nenv =
          {
            dcenv = env.dcenv; 
            tvenv = env.tvenv;
            vvenv = SM.add x ([], ([], t2)) (env.vvenv);
            ivenv = env.ivenv;
          } in 
          (
            match elaborate_expr nenv m1 with
              | (([],([],t1)) ,n1) -> ([],([],TArrow(t2, t1))), EFun(x, t2, n1)
              |_ -> Error.error [Error.Expr(e)] "Erreur"
          )
    | EApp(m1, m2) -> 
        (
          match (elaborate_expr env m1), (elaborate_expr env m2) with
            | (([],([],TArrow(t2, t1))), n1), (([],([],t2')), n2) when t2=t2'-> 
                ([], ([], t1)), (EApp(n1, n2))
            |_, _ -> Error.error [Error.Expr(e)] "Erreur"
        )
    | EConApp(c, lt, le) -> Error.error [Error.Expr(e)] "Not yet implemented"
    | ELet(eps, x, m0, m1) -> Error.error [Error.Expr(e)] "Ne semble pas etre\ 
                                explicitement typee"
    | ELetRec(l, m2) -> Error.error [Error.Expr(e)] "Erreur"
    | EMatch(eps, l) -> Error.error [Error.Expr(e)] "Erreur"
    | EImplicit(t) -> Error.error [Error.Expr(e)] "Erreur"
    | EFunI(eps, x, t, m) -> Error.error [Error.Expr(e)] "Erreur"
    | ELam(a,m) -> Error.error [Error.Expr(e)] "Erreur"
    | ETapp(m,t) -> Error.error [Error.Expr(e)] "Erreur"
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

