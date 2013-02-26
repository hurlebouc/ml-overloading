open Ast
open Type
open Matching
open Exproftype
open Printf
open Printast

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

(* Fonction donnant la bonne formation d'un type (avec les liaisons) 
 *
 * TODO utiliser les fonctions du prof plutôt que les miennes :
 *    - sch_as_XXX au lieu de mes match
 *    - close_scheme au lieu de bind_XXX_var
 *    _ Printast.print_XXX au lieu de print_XXX
 *
 * TODO vérifier que close_scheme ne doit pas être appliqué dans d'autres cas
 * que l'abstraction de type
 *
 * TODO vérifier que le test sur la restriction des définitions des types
 * implicites ne doit ne s'appliquer que dans les let
 *)

(* Deprecated *)
let rec bind_type_var env = function
  | TFvar(x) when SM.mem x env.tvenv -> TGvar(x)
  | TArrow(t1,t2) -> TArrow(bind_type_var env t1, bind_type_var env t2)
  | TConApp(tc, tl) -> TConApp(tc, bind_typelist_var env tl)
  | t -> t

(* Deprecated *)
and bind_typelist_var env = function
  | [] -> []
  | h::t -> (bind_type_var env h)::(bind_typelist_var env t);;

(* Deprecated *)
let bind_rg_var env = function
  | (tl,t) -> (bind_typelist_var env tl, bind_type_var env t);;

(* Deprecated *)
let bind_sch_var env = function
  | (tvl, rg) -> (tvl, bind_rg_var env rg);;


(* fonction dispatchant les variables dans les bons environnements *)

let rec make_new_env env = function
    | [] -> env
    | (None, x, s, _)::tl -> 
        let env' = {
          dcenv = env.dcenv; 
          tvenv = env.tvenv;
          vvenv = SM.add x ((*bind_sch_var env*) s) (env.vvenv);
          ivenv = env.ivenv;
        } in 
          make_new_env env' tl
    | (Some p, x, s, stub)::tl ->
        let rule = {
          priority = p;
          name = x;
          sch = (*bind_sch_var env*) s
        } in
        let env' = {
          dcenv = env.dcenv; 
          tvenv = env.tvenv;
          vvenv = env.vvenv;
          ivenv = Dn.add (env.ivenv) rule;
        } in
          (* On ajoute la varibales aux deux environnements *)
          make_new_env env' ((None, x, s, stub)::tl);;
  

(* fonction de substitution de types *)

exception Erreur of string

let rec substitution f g (x : type_variable) = match f, g with
    | h1::t1, h2::t2 -> if h1=x then h2 else substitution t1 t2 x
    | _ -> raise (Erreur "variable absente de la substitution");;

let rec substitution_partial f g (x : type_variable) = match f, g with
  | h1::t1, h2::t2 -> if h1=x then h2 else substitution_partial t1 t2 x
  | _ -> TGvar(x);;

let rec extract_unspec_var lv lt = match lv, lt with
  | _, [] -> lv
  | h1::t1, h2::t2 -> extract_unspec_var t1 t2
  | _ -> raise (Erreur "Trop de spécifications");;

(* renvoie true si s est un "bon" schema de type (respecte la restriction) *)
let test_restriction (s : sch) =
  let (tvs , (_, t)) = s in
  let rec is_in (x : type_variable) = function
    | TGvar(x') -> x=x'
    | TArrow(t1, t2) -> (is_in x t1) || (is_in x t2)
    | TConApp(_, tl) -> List.exists (is_in x) tl
    | _ -> false
  in
    List.for_all (fun x -> is_in x t) tvs

(* Fonction de test d'égalité de types 
 * deux schémas de types sont égaux s'il le sont par alpha-renommage et
 * permutations des abstractions de type
 *)

(* Deprecated *)
let rec equalsT t1 t2 = match t1, t2 with
    | TFvar(x), TFvar(y) -> x=y
    | TFvar(x), TGvar(y) -> x=y
    | TGvar(x), TFvar(y) -> x=y
    | TGvar(x), TGvar(y) -> x=y
    | TArrow(a, b), TArrow(a',b') -> (equalsT a a') && (equalsT b b')
    | TConApp(tc, tl), TConApp(tc', tl') -> (tc = tc') && (equalsTL tl tl')
    |_ -> false
(* Deprecated *)
and equalsTL l1 l2 = match l1, l2 with
    | [], [] -> true
    | h1::t1, h2::t2 -> (equalsT h1 h2) && (equalsTL t1 t2)
    | _ -> false;;

(* les deux fonctions précédentes sont issues d'une vielle version et sont
 * totalement fausses *)

(* Deprecated *)
let equalsT t1 t2 = size t1 = size t2 && Unification.unify t1 t2;;
(* Deprecated *)
let rec equalsTL l1 l2 = match l1, l2 with
    | [], [] -> true
    | h1::t1, h2::t2 -> (equalsT h1 h2) && (equalsTL t1 t2)
    | _ -> false;;
(* Deprecated *)
let equalsR r1 r2 = match r1, r2 with
  | (tl1, t1), (tl2, t2) -> (equalsT t1 t2) && (equalsTL tl1 tl2);;
(* Deprecated *)
let equalsS s1 s2 = match s1, s2 with
  | (tvl1, r1), (tvl2, r2) -> (List.length tvl1 = List.length tvl2) 
    && (equalsR r1 r2);;

(* Les versions précédendes sont peut-être justes mais dans le doute, on préfère
 * la suivante *)

let equalsS s1 s2 = match s1, s2 with
  | (tvl1, (tl1, t1)), (tvl2, (tl2, t2)) -> (List.length tvl1 = List.length tvl2) 
    && let subs = List.combine tvl1 tvl2 in
    let g x = TGvar (snd (List.find (fun (x',y') -> x'=x) subs)) in
    let f x = TFvar(x) in
    let (tl1', t1') = (List.map (Type.lift f g) tl1), (Type.lift f g t1) in
      t2=t1' && tl1'=tl2;;




(* DEBUG : fonction d'impression de type, rang et schéma *)

let rec to_string_details = function
  | TFvar(x) -> String.concat "" ["TFvar("; x; ")"]
  | TGvar(x) -> String.concat "" ["TGvar("; x; ")"]
  | TArrow(t1, t2) -> String.concat "" ["("; to_string_details t1 ; ")->("; to_string_details t2]
  | TConApp(_,_) -> "cons";;

let print_type t = printf "(%s)" (to_string_details t) 
let print_rg = function
  |(r, t) -> List.iter print_type r; printf "(%s)" (to_string_details t); ;;

let rec print_sch : sch -> unit = function
  | ([], rg) -> print_rg rg
  | (x::tl, rg) -> printf "'%s " x; print_sch (tl, rg);;

(* ------------------------------------------------------------------------- *)

let rec elaborate_expr env (e : expression) : sch * expression =

  (* fonction de résolution des arguments implicites *)

  let rec resolve_imp env (s : sch) (n : expression) = 
    (*printf "%s : %s\n" (Printast.expr_to_string n) (Printast.sch_to_string
     * s);*)
    match s with
      | [], (tau::rho, t) -> 
          let n0 = try exproftype env.ivenv tau with
            | Dn.ElabFail message -> Error.error [Error.Expr(e)] message
          in
          let s' = ([], (rho, t)) in
          let n' = EApp(n, n0) in
            resolve_imp env s' n'
      |_ -> s, n
  in

 (*printf "expression : %s\n\n" (Printast.expr_to_string e); *)

  match e with
    | EVar(x)->
        let tv, r = (
          try
            SM.find x env.vvenv
          with
            (*| _ -> try Dn.get env.ivenv x with*)
            
            (* Les variables implicites sont aussi dans l'environnement
             * explicite*)
            
            | _ -> Error.error [Error.Expr(e)] "unexpected..."
        )
        in
        (*let (tv,r) = close_scheme tv r in*)
          (try resolve_imp env (tv,r) e with
            | Dn.ElabFail message -> Error.error [Error.Expr(e)] message)
    | EFun(x, t2, m1)-> 
        (*printf "TEST : %s\n" (to_string_details t2);*)
        (*let t2 = bind_type_var env t2 in*)
        let nenv = {
          dcenv = env.dcenv; 
          tvenv = env.tvenv;
          vvenv = SM.add x ([], ([], t2)) (env.vvenv);
          ivenv = env.ivenv;
        } in (
          match elaborate_expr nenv m1 with
            | (([],([],t1)) ,n1) -> ([],([],TArrow(t2, t1))), EFun(x, t2, n1)
            |_ -> Error.error [Error.Expr(e)] "On attend ici un type simple"
        )
    | EApp(m1, m2) -> (
        match (elaborate_expr env m1), (elaborate_expr env m2) with
          | (([],([],TArrow(t2, t1))), n1), (([],([],t2')), n2) (*when t2=t2'*)->
              (*if equalsT t2 t2' then*)
              if t2=t2' then
                (* Le "=" nest pas trop restrictif car les termes sont 
                 * explicitement spécifiés *)

                ([], ([], t1)), (EApp(n1, n2))
              else begin 
                printf "Le type de l'argument est %s\n" (to_string t2');
                printf "mais %s est attendu\n"  (to_string t2);
                Error.error [Error.Expr(e)] "Types incompatibles"
              end
          |(([],([],TArrow(_, _))), _), (s2,_) -> 
              printf "L'expression %s\nest de type [%s]\n"
                (expr_to_string m2) (sch_to_string s2);
              Error.error [Error.Expr(e)] "Le type de l'argument doit être simple"
          | (s1, _), _ -> 
              printf "L'expression %s\nest de type [%s]\n"
                (expr_to_string m1) (sch_to_string s1);
              Error.error [Error.Expr(e)] "Le type de la fonction n'est pas une flèche"
      )
    | EConApp(c, lt, le) ->
        (*let lt = bind_typelist_var env lt in*)
        let Wf.Scheme(tvl, lte, tdef) = SM.find c (env.dcenv) in
        (* On sait |lt| = |tvl| car sinon le test de well foundness aurait
         * planté *)
        let g = substitution tvl lt in
        let f x = Error.error [Error.Expr(e)] "Ce ne sont pas ces variables que vous recherchez..." in
        let rec comp_arg le lte = match le, lte with
          | h1::t1, h2::t2 -> (
              match elaborate_expr env h1 with
                (*| ([], ([], t)), n -> if equalsT t (Type.lift f g h2)*)
                | ([], ([], t)), n -> if t=(Type.lift f g h2)

                  (* De même ici *)

                  then n::(comp_arg t1 t2)
                  else (
                    printf "Le type utilisé est \t\t%s\n" (to_string t);
                    printf "alors que le type spécifié est \t%s\n"  (to_string (Type.lift f g h2));
                    Error.error [Error.Expr(h1)] "Types incompatibles"
                  )
                | _ -> Error.error [Error.Expr(h1)] "On attend ici un type simple" 
            )
          |[], [] -> []
          |_ -> Error.error [Error.Expr(e)] "Les arités ne sont pas respectées"
        in
        let le' = comp_arg le lte in
        let tc = match tdef with
          | TConApp(name, _) -> name
          |_ -> Error.error [Error.Expr(e)] "unexpected..."
        in
          ([], ([], TConApp(tc, lt))), EConApp(c, lt, le')
    | ELet(p, x, m1, m2) -> 
        let (s1, n1) = elaborate_expr env m1 in
          if not (test_restriction s1)
          then
            Error.error [Error.Expr(e)] 
              ("Le type ["^ (Printast.sch_to_string s1) ^ "] ne peut être implicite")
          else 
            let nenv = try make_new_env env [(p, x, s1, None)] with
              | Dn.AddFail (x',s') -> 
                  printf "La variable %s est de type [%s]\n" 
                    x (sch_to_string s1);
                  printf "et la variable %s de type [%s] est déja présente\n"
                    x' (sch_to_string s');
                  Error.error [Error.Expr(e)] ("Chevauchement de définitions implicites")
            in
            let (s2, n2) = elaborate_expr nenv m2 in 
              (s2, ELet(None, x, n1, n2))
    | ELetRec(l, m2) ->
        let nenv = make_new_env env l in
        let rec elab accu = function
          | [] -> accu
          | (_, x, s1, m1)::tl ->
              if not (test_restriction s1)
              then 
                Error.error [Error.Expr(e)] 
                  ("Le type ["^ (Printast.sch_to_string s1) ^ "] ne peut être implicite")
              else
                (*let s1 = bind_sch_var env s1 in*)
                let (s1', n1) = elaborate_expr nenv m1 in
                  if not (equalsS s1 s1') then begin
                    (*if s1<>s1' then begin*)
                    (*let (_, (_, t1)) = s1 in
                    let (_, (_, t2)) = s1' in
                      printf "TEST unifiable : %B\n" (Unification.unify t1 t2);*)
                      printf "le type utilisé est \t\t";
                      (*print_sch s1';*)
                      printf "%s" (Printast.sch_to_string s1');
                      printf "\nalors que le type spécifié est \t";
                      (*print_sch s1;*)
                      printf "%s" (Printast.sch_to_string s1);
                      printf "\n";
                      Error.error [Error.Expr(m1)] "Types incompatibles" 
                  end

                  (* De même ici *)

                  else
                    let accu' = (None, x, s1, n1)::accu in
                      elab accu' tl
        in
        let res = elab [] l in
        let (s2, n2) = elaborate_expr nenv m2 in 
          (s2, ELetRec(res, n2))
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
              (*let tauibarre = bind_typelist_var env tauibarre in*)
                (*if not (equalsTL tauibarre taubarre) then*)
                if tauibarre<>taubarre then
                  Error.error [Error.Expr(e)] "spécifications incompatibles"

                (* vérification de la cohérence des codomaines *)

                else let Wf.Scheme(alphabarre, tibarre, t) = SM.find dc (env.dcenv) in
                  match t with
                    | TConApp(tc', _) -> if tc'<>tc then
                        Error.error [Error.Expr(e)] "codomaines incompatibles"

                      (* vérification de la cohérence du type des branches *)

                      (* On sait ici que |alphabarre| = |taubarre| car sinon le
                       * test de well-foundness aurait planté *)

                      else let g = substitution alphabarre taubarre in
                      let f x = Error.error [Error.Expr(e)] "unexpected..." in
                      let rec make_new_env_from_var = function
                        | [], [] -> env
                        | xij::tailV, tij::tailT -> 
                            let nenv' = make_new_env_from_var (tailV, tailT) in
                            let tij' = Type.lift f g tij in
                              make_new_env nenv' [None, xij, ([], ([], tij')), None]
                                (* Le test de laison n'est pas obligatoire ici *)
                        |_ -> Error.error [Error.Expr(e)] "problème d'arité"
                      in 
                      let nenv = make_new_env_from_var (xibarre, tibarre) in
                      let (si, ni) = elaborate_expr nenv mi in
                        if tail = [] then si, [Branch(PConApp(dc, taubarre, xibarre), ni)] 
                        else let (s, lp')  = test_spec tail in
                          if not (equalsS s si) then
                          (*if s<>si then*)
                            Error.error [Error.Expr(e)] "types de branches incompatibles"
                          else 
                              s, Branch(PConApp(dc, taubarre, xibarre), ni)::lp'
                    |_ ->  Error.error [Error.Expr(e)] "unexpected..."
        in
        let (s, lp') = test_spec lp in
          (s, EMatch(n, lp'))
    | EImplicit(t) ->
        (*let t = bind_type_var env t in*)
        let n = try exproftype env.ivenv t with
          | Dn.ElabFail message -> Error.error [Error.Expr(e)] message
        in
          ([], ([], t)), n
    | EFunI(p, x, t, m) -> 
        (*let t = bind_type_var env t in*)
        let nenv = make_new_env env [Some p, x, ([], ([], t)), None] in
        let (s, n) = elaborate_expr nenv m in (
          match s with
          | ([], (r, tl)) -> ([], (t::r, tl)), EFun(x, t, n)
          |_ -> Error.error [Error.Expr(m)] "Cette expression ne peut être polymorphe."
        )
    | ELam(a,m) ->
        let rec make_new_env_from_TV = function
          | [] -> env
          | hd::tl -> 
              let nenv' = make_new_env_from_TV tl in {
                dcenv = nenv'.dcenv; 
                tvenv = SM.add hd () (nenv'.tvenv);
                vvenv = nenv'.vvenv;
                ivenv = nenv'.ivenv;
              } 
        in
        let nenv = make_new_env_from_TV a in
        let (s, n) = elaborate_expr nenv m in 
        let a', r = s in
          (close_scheme (a@a') r, ELam(a,n))

    (*(
     match s with
     |[], r -> ((a, r), ELam(a,n))
     |_ -> Error.error [Error.Expr(e)] "Polymorphisme non permis ici"
     )*)    
    | ETapp(m,lt) ->
        (*let lt = bind_typelist_var env lt in*)
        let (la, r), n = elaborate_expr env m in
          if List.length lt > List.length la 
          then
            let str = (string_of_int (List.length lt)) ^ " types sont donnés "^
                      "alors que "^(string_of_int (List.length la))^" sont attendus." in
              Error.error [Error.Expr(e)] str
              else
                (*(* DEBUG *) print_sch (la, r); printf "\n";*)
                (*(* DEBUG *) List.iter (fun x -> printf "%s " (to_string x)) lt; printf "\n";*) 
                let g = substitution_partial la lt in
                let lift_row = List.map (Type.lift (fun x -> TFvar(x)) g) in
                let (timp, t) = r in
                let sublist = extract_unspec_var la lt in
                let s' = (sublist, ((lift_row timp), Type.lift (fun x -> TFvar(x)) g t)) in
                let n' = ETapp(n, lt) in
                  resolve_imp env s' n'
(*([], ((lift_row timp), Type.lift (fun x -> TFvar(x)) g t)), ETapp(n, lt)*)

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

