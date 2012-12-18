open Printf
open Ast

(* Auxiliary function to type-check a program *)


(* ------------------------------------------------------------------------- *)

(* Read the file and parse the program. *)

let Program (_, main) as program =
  let channel = open_in Settings.filename in
  let lexbuf = Lexing.from_channel channel in
  lexbuf.Lexing.lex_curr_p <-
      { 
	Lexing.pos_fname = Settings.filename; 
	Lexing.pos_lnum  = 1;
	Lexing.pos_bol   = 0; 
	Lexing.pos_cnum  = 0
      };
  try
    let program = Parser.program Lexer.main lexbuf in
    close_in channel;
    program
  with Parser.Error ->
    Error.error
      [ Error.Loc
          (Some (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)) ]
      "Syntax error."

(* Check well-formedness and perform alpha-conversion. *)
module W = struct
  let Program (prelude, main) = program 
  let dcenv, program = Wf.wf_program program
  let Program (_, iml) = program
  let _ = 
    if Settings.show_alpha then begin
      Settings.log 1 Format.printf "*** Program after alpha-conversion:@.";
      Printast.print_program false Format.std_formatter program
    end
end

(* Recheck the program as a sanity check *)
let _,_ = Wf.wf_program W.program

(* typecheck and elaborate the program *)
module E = struct
  let sch, ml = Elaborate.elaborate_expr W.dcenv W.iml
  let program = Program (W.prelude, ml)
end

(* Output the elaborated program as OCaml source *)
let () =
  Settings.log 1 printf "*** The program after elaboration:\n";
  if Filename.check_suffix Settings.filename ".iml" then
    let outputname = Filename.chop_suffix Settings.filename ".iml" ^ ".ml" in
    let outchan = open_out outputname in
    let fmt = Format.formatter_of_out_channel outchan in
    Printast.print_program true fmt E.program;
    close_out outchan
  else
    Error.error []
      (sprintf "Don't know what to do with filename: %s"
         (Filename.basename Settings.filename))

