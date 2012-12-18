(* Prepare for parsing the command line. *)

let filename = ref None
let insert name = filename := Some name
let verbose = ref 0
let log n f x = if !verbose > n then f x
let alpha = ref false


let options = Arg.align [
  "-v", Arg.Unit (fun() -> incr verbose), "increase verbosity";
  "-alpha", Arg.Set alpha, "print alpha-converted program";
]

let usage =
  Printf.sprintf "Usage: %s <options> <filename>" Sys.argv.(0)

(* Parse the command line. *)

let () =
  Arg.parse options insert usage

(* Export the settings. *)

let filename =
  match !filename with
  | None ->
      Arg.usage options usage;
      exit 1
  | Some filename ->
      filename

let show_alpha = !alpha
