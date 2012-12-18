open Printf
open Lexing

(* A location gives the location of an error *)
type location =
    (* Exact location in the source program *)
  | Loc of (Lexing.position * Lexing.position) option
    (* [Definition (kind, name)] designate the enclosing [kind] Definition of [name] *)
  | Definition of string * string 
    (* [Binding name] designate the enclosing binding for [name] *)
  | Binding of string
    (* [Expr expressions] give [expression] as the location of the error *)
  | Expr of Ast.expression

let print_location = function
  | Loc (Some (pos1, pos2)) ->
      let file = pos1.pos_fname in
      let line = pos1.pos_lnum in
      let char1 = pos1.pos_cnum - pos1.pos_bol in
      let char2 = pos2.pos_cnum - pos1.pos_bol in
        (* intentionally [pos1.pos_bol] *)
      fprintf stderr "File \"%s\", line %d, characters %d-%d:\n"
        file line char1 char2
	(* use [char1 + 1] and [char2 + 1] if *not* using Caml mode *)
  | Loc None ->
      ()
  | Definition (kind, x)  ->
      fprintf stderr "In %s definition %s:\n" kind x
  | Binding x ->
      fprintf stderr "In binding %s:\n" x
  | Expr expr ->
      Format.fprintf Format.err_formatter
        "@[<hv 2>In expression:@ %a@]@."
        (Printast.print_expr false) expr

let signaled =
  ref false

let warn locs message =
  List.iter print_location locs;
  fprintf stderr "%s\n%!" message

let signal locs message =
  List.iter print_location locs;
  fprintf stderr "%s\n%!" message;
  signaled := true

let error locs message =
  signal locs message;
  exit 1

let signaled () =
  if !signaled then
    exit 1

exception Error of location list * string

(* Raises an message without repporting the error immediately 
   where location for errors can be printed first *)

let delay locs message = raise (Error (locs, message))

(* [at_location locs f x] evaluates f x and reraises errors with additional
   location information.  *)

let at_location locs f x =
  try f x 
  with Error (locs', message) -> delay (locs @ locs') message

(* [handle f x] evaluates f x and repport errors immediately, with current
   location information. *)

let handle f x =
  try f x
  with Error (locs, message) -> error locs message


