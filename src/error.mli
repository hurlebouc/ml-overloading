(* This module helps report errors or informational messages. *)

(* This the type of source code locations. A location delimits a range of
   text in a source file. *)

type location =
  | Loc of (Lexing.position * Lexing.position) option
  | Definition of string * string
  | Binding of string
  | Expr of Ast.expression

(* [error locs msg] displays the error message [msg], referring to the
   locations [locs], and stops the program. [signal locs msg] is
   analogous, but does not stop the program. *)

val warn: location list -> string -> unit
val error: location list -> string -> 'a
val signal: location list -> string -> unit

(* [signaled()] stops the program if [signal] or [signalv] was
   previously called. *)

val signaled: unit -> unit

exception Error of location list * string

val delay : location list -> string -> 'a
val at_location : location list -> ('a -> 'b) -> 'a -> 'b
val handle : ('a -> 'b) -> 'a -> 'b
 
