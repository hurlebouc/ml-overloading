open Ast
open Type
open Matching
open Exproftype
open Printf

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
  let rec substitution f g (x : type_variable) = match f, g with
    | h1::t1, h2::t2 -> if h1=x then h2 else substitution t1 t2 x
    | _ -> Error.error [Error.Expr(e)] "Erreur ici"
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
          let Wf.Scheme(tvl, lte, t) = SM.find c (env.dcenv) in
          let g = substitution tvl lt in
          let f x = Error.error [Error.Expr(e)] "unexpected..." in
          let rec comp_arg le lte = match le, lte with
            | h1::t1, h2::t2 -> (
                  match elaborate_expr env h1 with
                    | ([], ([], t)), n -> if t=(Type.lift f g h2)
                      then n::(comp_arg t1 t2)
                      else (
                        printf "Le type utilisé est %s\n" (to_string t);
                        printf "alors que le type spécifié est %s\n"  (to_string (Type.lift f g h2));
                        Error.error [Error.Expr(e)] "Types incompatibles"
                      )
                    | _ -> Error.error [Error.Expr(e)] "On attend ici un type simple"
              (* De même ici *)
              )
            |[], [] -> []
            |_ -> Error.error [Error.Expr(e)] "Les arités ne sont pas respectées"
          in
          let le' = comp_arg le lte in
          let tc = match t with
            | TConApp(name, _) -> name
            |_ -> Error.error [Error.Expr(e)] "unexpected..."
          in
            ([], ([], TConApp(tc, lt))), EConApp(c, lt, le')
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
      | EMatch(m, lp) -> 
          
          (* vérification du type de M*)

          let (s, n) = elaborate_expr env m in
          let (tc, taubarre) = match s with 
            | [], ([], TConApp(tc, taubarre)) -> (tc, taubarre)
            |_ -> Error.error [Error.Expr(m)] "L'expression devrait être typée comme constructeur de type"
          in
          let rec test_spec = function
            | [] -> Error.error [Error.Expr(e)] "matching sans branche"
            (*| [] -> []*)
            | hd::tail -> 

                (* vérification de la cohérence des spécifications *)

                let Branch(PConApp(dc, tauibarre, xibarre), mi) = hd in
                  if tauibarre <> taubarre then
                    Error.error [Error.Expr(e)] "spécifications incompatibles"
                  
                  (* vérification de la cohérence des codomaines *)
                  
                  else let Wf.Scheme(alphabarre, tibarre, t) = SM.find dc (env.dcenv) in
                    match t with
                      | TConApp(tc', _) -> if tc'<>tc then
                          Error.error [Error.Expr(e)] "codomaines incompatibles"

                        (* vérification de la cohérence du type des branches *)

                        else let g = substitution alphabarre taubarre in
                        let f x = Error.error [Error.Expr(e)] "unexpected..." in
                        let rec make_new_env_from_var = function
                          | [], [] -> env
                          | xij::tailV, tij::tailT -> 
                              let nenv' = make_new_env_from_var (tailV, tailT) in
                              let tij' = Type.lift f g tij in
                                make_new_env nenv' [None, xij, ([], ([], tij')), None]
                          |_ -> Error.error [Error.Expr(e)] "problème d'arité"
                        in 
                        let nenv = make_new_env_from_var (xibarre, tibarre) in
                        let (si, ni) = elaborate_expr nenv mi in
                          if tail = [] then si, [Branch(PConApp(dc, taubarre, xibarre), ni)] 
                          else let (s, lp')  = test_spec tail in
                            if s <> si then
                              Error.error [Error.Expr(e)] "types de branches incompatibles"
                            else 
                              s, Branch(PConApp(dc, taubarre, xibarre), ni)::lp'
                      |_ ->  Error.error [Error.Expr(e)] "unexpected..."
          in
          let (s, lp') = test_spec lp in
            (s, EMatch(n, lp'))





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

