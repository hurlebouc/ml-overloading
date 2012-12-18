open Ast

val elaborate_expr : Wf.data_constructor_table -> expression -> sch * expression
