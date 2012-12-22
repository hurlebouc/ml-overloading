open Ast
open Type
open Matching
open Exproftype

(* ------------------------------------------------------------------------- *)

(* The elaborator receives a term that has been alpha-converted so that
   bindings should not shadow one another.

   It uses separate environments [tvenv] to map type variables to unit,
   [vvenv] to map value variables to type schemes and [ivenv] to keep a
   structure of avaivalue implicit value variables with their type schemes
   of type [Exproftype.I.t] *)

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
   (* INCOMPLETE! of course something should be done here... *)
   failwith "NOT IMPLEMENTED"

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

