(** LISA Is A Static Analyser
    Jonathan Laurent - 2014
*)

type domain = 
| Consts
| Intervals
| Polyhedra

let test_mode                     = ref false
let domain                        = ref Polyhedra
let widening_delay                = ref 3
let unrolling_factor              = ref 6
let decreasing_iterations         = ref 3
let auto_phantom_vars             = ref false
let integer_mode                  = ref true
let verbose_unrolled_statements   = ref true

let ifiles                  = Queue.create ()

let options = [
  
  ("--consts", Arg.Unit (fun () -> domain := Consts),
   "  Use the rationnal constants domain");

  ("--intervals", Arg.Unit (fun () -> domain := Intervals),
   "  Use the intervals domain. See option -q and -i");

  ("--polyhedra", Arg.Unit (fun () -> domain := Polyhedra),
   "  Use the polyhedra domain from the Apron library [Default]");

  ("-i", Arg.Set integer_mode,
   "  Variables are integers [Default]");

  ("-r", Arg.Set integer_mode,
   "  Variables are rationnals");


  ("--widening-delay", Arg.Int (fun i -> widening_delay := i),
   "  Set the widening delay for loops [Default : 3]");

  ("--unrolling-factor", Arg.Int (fun i -> unrolling_factor := i),
   "  Set the unrolling factor for loops [Default : 6]");

  ("--decreasing-iterations", Arg.Int (fun i -> decreasing_iterations := i),
   "  Set the number of decreasing iterations for loops analysis [Default : 3]");

  ("--auto-phantom-vars", Arg.Set auto_phantom_vars,
   "  Use phantom vars for loop analysis");

  ("--no-auto-phantom-vars", Arg.Clear auto_phantom_vars,
   "  Don't use phantom vars for loop analysis [Default]");

  ("--verbose-unrolled-statements", Arg.Set verbose_unrolled_statements,
    " Print assertions failures and print messages for unrolled statements [Default]"
  );

  ("--no-verbose-unrolled-statements", Arg.Clear verbose_unrolled_statements,
    " Don't print assertions failures and print messages for unrolled statements"
  );

  ("--test", Arg.Set test_mode,
   "  Run the standard tests");

]

let usage = "usage: lisa [--test] [DOMAIN] [PARAMS] FILES"

let anon_args s = Queue.push s ifiles



module Consts = Consts.M
module IIntervals = Intervals.Make (Intervals.Integers)
module RIntervals = Intervals.Make (Intervals.Rationals)

module IINRel = Non_relational.Make (IIntervals)
module RINRel = Non_relational.Make (RIntervals)
module CNRel  = Non_relational.Make (Consts)

module Polyhedra = Polyhedra.Make (Polyhedra.Standard)


let line = "--------------------------------------------------" 

let doit run_analysis filename =

  let open Format in
  let open Filename in

  let ff = std_formatter in
  let linej () = fprintf ff "\n\n" in
  let endl () = fprintf ff "\n" in
  let print s = fprintf ff "%s" s in
  let prog = File_parser.parse_file filename in

  let domain_name = match !domain with 
    | Polyhedra -> "polyhedra" 
    | Consts    -> "constants"
    | Intervals -> "intervals"
  in

  let reference_file = 
    (dirname filename) ^ dir_sep ^ parent_dir_name ^ 
      dir_sep ^ "results" ^ dir_sep ^
      (chop_extension (basename filename)) ^ "." ^ domain_name ^ ".txt"
  in

  print filename; linej ();

  if !test_mode then 
    begin
      Abstract_syntax_printer.print_prog ff prog;
      linej (); print "OUR RESULT:"; endl ()
    end;
      
  run_analysis ff prog;

  if !test_mode && Sys.file_exists reference_file then
    begin
      linej (); print "REFERENCE:"; endl ();
      Utils.print_file ff reference_file
    end;
      
  linej(); print line; linej()
  

let main () = 

  Arg.parse options anon_args usage;

  let module Params = struct
    let widening_delay = !widening_delay
    let unrolling_factor = !unrolling_factor
    let decreasing_iterations = !decreasing_iterations
    let auto_phantom_vars = !auto_phantom_vars

    let verbose_unrolled_statements = !verbose_unrolled_statements

  end in

  let run_analysis = 

    match !domain with
    | Consts -> 
      let module Analysis = Analysis.Make (CNRel) (Params) in
      Analysis.run
    | Intervals when !integer_mode ->
      let module Analysis = Analysis.Make (IINRel) (Params) in
      Analysis.run
    | Intervals ->
      let module Analysis = Analysis.Make (RINRel) (Params) in
      Analysis.run
    | Polyhedra ->
      let module Analysis = Analysis.Make (Polyhedra) (Params) in
      Analysis.run

  in
  try
    Format.fprintf Format.std_formatter "\n%s\n\n" line;
    Queue.iter (doit run_analysis) ifiles 
  with
  | _ -> prerr_string "An internal error occured.\n " ; exit 2




let _ = main ()
