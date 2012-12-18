(* Pretty-printer for abstract syntax *)

open Format
open Ast

let print_gtyp = Type.print Type.print_gvar
let print_ftyp = Type.print Type.print_fvar
let print_gtyp0 = Type.print0 Type.print_gvar
let print_ftyp0 = Type.print0 Type.print_fvar

let print_typ ml =
  (* ml print free type variables as abstract types *)
  if ml then print_ftyp else print_gtyp

let print_sch ml pp (tvs, (ts, t)) =
  fprintf pp "@[<hov 1>";
  (* print bound variables *)
  let print pp = if ml then fprintf pp "%a@ " else fprintf pp "%a.@ " in
  List.iter (print pp Type.print_gvar) tvs;
  if ml && tvs <> [] then fprintf pp ".@ ";
  (* print row *)
  let arrow = if ml then "->" else "=>" in
  List.iter (fun t -> fprintf pp "%a@ %s@ " (print_typ ml) t arrow) ts;
  fprintf pp "%a@]" (print_typ ml) t


let string_of_I = function
    None -> ""
  | Some u ->
      match u with 
      | Low -> "?<"
      | Normal -> "?"
      | High -> "?>"

let sch_to_string sch =
  print_sch false str_formatter sch; flush_str_formatter()


let print_mode pp mode =
  if mode then fprintf pp "@.;;@."
  else fprintf pp "in"
  

let rec print_expr_0 ml pp = function
  | EVar x -> fprintf pp "%s" x
  | EConApp(cstr, [], []) -> fprintf pp "%s" cstr
  | EImplicit _ when ml -> assert false
  | EImplicit ty  ->
      fprintf pp "(@[<hov 2>?@ : %a@])" print_gtyp ty
  | ETapp (a, _) when ml -> print_expr_0 ml pp a
  | ETapp (a, []) -> assert false
  | ETapp (a, t::ts) ->
      fprintf pp "@[<hov 2>%a@,[%a" (print_expr_0 ml) a print_gtyp t;
      List.iter (fprintf pp ",@ %a" print_gtyp) ts;
      fprintf pp "]@]@]";
  | a -> fprintf pp "@[<hov 1>(%a)@]" (print_expr ml) a

and print_expr_1 ml pp = function
  | EApp(_, _) as a ->
      fprintf pp "@[<hov 2>";
      let rec print_app pp = function
        | EApp(a, b) ->
            fprintf pp "%a@ %a" print_app a (print_expr_0 ml) b
        | a -> 
            print_expr_0 ml pp a in
      print_app pp a;
      fprintf pp "@]"
  | EConApp(cstr, ts, al) ->
      fprintf pp "%s" cstr;
      begin match ts with
      | [] -> ()
      | t :: ts when  ml -> ()
      | t :: ts -> 
          fprintf pp "@[<hov 0>[%a" print_gtyp t;
          List.iter (fun t -> fprintf pp ",@ %a" print_gtyp t) ts;
          fprintf pp "]@]";
      end;
      begin match al with
      | [] -> ()
      | a :: al -> 
          fprintf pp "@[<hov 1>(%a" (print_expr ml) a;
          List.iter (fun a' -> fprintf pp ",@ %a"( print_expr ml) a') al;
          fprintf pp ")@]"
      end
  | a ->
      print_expr_0 ml pp a

and print_definition ml pp prefix (u, x, t, a) =
  if ml && u <> None then assert false; 
  fprintf pp "@[<hv 2>%s %s%s : %a =@ %a@]"
    prefix (string_of_I u) x (print_sch ml) t (print_expr ml) a

and print_abs ml pp a =
  let rec print pp = function
    | EFun (x, t, a) when ml -> 
        fprintf pp "fun %s ->@ %a" x print a
    | EFun (x, t, a) -> 
        fprintf pp "fun (%s : %a) ->@ %a" x print_gtyp t print a
    | EFunI (q, x, t, a) when ml -> assert false
    | EFunI (q, x, t, a) -> 
        fprintf pp "fun (%s%s : %a) =>@ %a"
          (string_of_I (Some q)) x print_gtyp t print a
    | ELam ([], a) -> print pp a
    | ELam (tv::tvs, a) ->
        fprintf pp "@[<hov 2>@[<hov 2>";
        if ml then begin
          fprintf pp "fun (type %s)" tv;
          List.iter (fprintf pp "@ (type %s)") tvs;
          fprintf pp "@] ->"
        end else begin
          fprintf pp "Lam '%s" tv;
          List.iter (fprintf pp "@ '%s") tvs;
          fprintf pp "@]."
        end;
        fprintf pp "@ %a@]" print a
    | a ->
        fprintf pp "@]@ %a" (print_expr ml) a in
  fprintf pp "@[<hv 2>@[<hov 0>%a@]" print a
    

and print_expr_mode mode ml pp = function
  | EFun(_, _, _) 
  | EFunI(_, _, _, _) 
  | ELam (_, _) as a ->
      print_abs ml pp a
  | ELetRec ([], a) ->
      print_expr ml pp a
  | ELetRec (b::bs, a) -> 
      fprintf pp "@[<hv 0>"; 
      print_definition ml pp "let rec" b;
      List.iter (print_definition ml pp "and") bs; 
      fprintf pp "@ %a@ %a@]" print_mode mode (print_expr_mode mode ml) a
  | ELet(Some u, x, a, b) when ml -> assert false
  | ELet(u, x, a, b) ->
      fprintf pp "@[<hov 0>@[<hv 2>let %s%s =@ %a@] %a@ %a@]"
        (string_of_I  u) x (print_expr ml) a
        print_mode  mode (print_expr_mode mode ml) b
  | EMatch(a, bl) ->
      fprintf pp "@[@[<hv 2>%s %a with"
        (if ml then "begin match" else "match") (print_expr ml) a;
      List.iter (print_branch ml pp) bl;
      fprintf pp "@]@ end@]" ;
  | a ->
      print_expr_1 ml pp a

and print_expr ml = print_expr_mode false ml

and print_branch ml pp (Branch(pat, expr)) =
  fprintf pp "@ @[<hov 4>| %a ->@ %a@]"
    (print_pat ml) pat (print_expr ml) expr

and print_pat ml pp (PConApp(cstr, ts, args)) =
  match ts, args with
  | [], [] ->
      fprintf pp "%s" cstr
  | ts, al ->
      fprintf pp "%s@[<hov 0>" cstr;
      if not ml then
        List.iter (fun t -> fprintf pp "@ [%a]" print_gtyp t) ts;
      fprintf pp "@]";
      begin match al with
      | [] -> ()
      | a :: al -> 
          fprintf pp "@[<hov 1>(%s"  a;
          List.iter (fun a' -> fprintf pp ",@ %s" a') al;
          fprintf pp ")@]"
      end

let print_data_constructor_definition pp (DefCon(cstr, args)) =
  match args with
  | [] ->
      fprintf pp "%s" cstr
  | t1 :: tl ->
      fprintf pp "%s of @[<hov 0>%a" cstr print_gtyp0 t1;
      List.iter (fun t -> fprintf pp " *@ %a" print_gtyp0 t) tl;
      fprintf pp "@]"

let print_data_type_definition pp (DefDataType(vars, tc, dcl)) =
  fprintf pp "@[<v 2>type ";
  begin match vars with
  | [] -> ()
  | [v1] -> fprintf pp "'%s " v1
  | v1 :: vl ->
      fprintf pp "('%s" v1;
      List.iter (fprintf pp ", '%s") vl;
      fprintf pp ") "
  end;
  fprintf pp "%s =" tc;
  List.iter
    (fun dc -> fprintf pp "@ | %a" print_data_constructor_definition dc)
    dcl;
  fprintf pp "@]@.@."

let print_program ml fmt (Program(decls, body)) =
  fprintf fmt "@[<v 0>";
  List.iter (print_data_type_definition fmt) decls;
  fprintf fmt "@;;;@;%a@]@.\n" (print_expr_mode true ml) body

let expr_to_string  expr =
  print_expr false str_formatter expr; flush_str_formatter()
