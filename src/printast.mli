(* This module provides a pretty-printer to display abstract syntax
   trees in concrete syntax. *)

val string_of_I : Ast.priority option -> string
val print_ftyp: Format.formatter -> Ast.typ -> unit
val print_gtyp: Format.formatter -> Ast.typ -> unit
val print_sch: bool -> Format.formatter -> Ast.sch -> unit
val sch_to_string : Ast.sch -> string
val print_expr: bool -> Format.formatter -> Ast.expression -> unit
val expr_to_string : Ast.expression -> string

val print_program: bool -> Format.formatter -> Ast.program -> unit

