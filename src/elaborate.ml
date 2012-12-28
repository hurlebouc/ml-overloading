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
    | EVar(x) -> 
        (
          try (StringMap.find x (env.vvenv)), EVar(x) with
            | Not_found -> Error.error [Error.Expr(EVar(x))] "Erreur"
            | _ -> Error.error [Error.Expr(EVar(x))] "Erreur"
        )
    | EFun(x, t2, m1) -> 
        let nenv =
          {
            dcenv = env.dcenv; 
            tvenv = env.tvenv;
            vvenv = SM.add x ([], ([], t2)) (env.vvenv);
            ivenv = env.ivenv;
          } in 
          (
            match elaborate_expr nenv m1 with
              | (([], ([], t1)) ,n1) -> ([], ([], TArrow(t2, t1))), EFun(x, t2, n1)
              |_ -> Error.error [Error.Expr(EFun(x, t2, m1))] "Erreur"
          )
    | _ -> failwith "ELABORATION NOT IMPLEMENTED"
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

