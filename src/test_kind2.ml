
let modelcheck _ = ()

let simulate () = ()

let convert () = ()

let help () = ()

(* 
   
   kind2 modelcheck

   kind2 simulate 

   kind2 convert

   kind2 help

*)


type smtsolver = [ `Z3_SMTLIB | `CVC4_SMTLIB ]

let pp_print_smtsolver ppf = 
  let p = Format.fprintf ppf "%s" in
  function
    | `Z3_SMTLIB -> p "Z3"
    | `CVC4_SMTLIB -> p "CVC4"

type smtlogic = [ `detect | `LIA | `LRA ]

let pp_print_smtlogic ppf = 
  let p = Format.fprintf ppf "%s" in
  function
    | `detect -> p "detect"
    | `LIA -> p "LIA"
    | `LRA -> p "LRA"

type kind2_module = [`PDR | `BMC | `IND | `INVGEN ]

let pp_print_kind2_module ppf = 
  let p = Format.fprintf ppf "%s" in
  function
    | `PDR -> p "PDR"
    | `BMC -> p "BMC"
    | `IND -> p "k-induction"
    | `INVGEN -> p "invariant generation"

type copts =
  { timeout_wall : int;
    timeout_virtual : int;
    smtsolver : smtsolver;
    smtlogic : smtlogic;
    z3_bin : string;
    cvc4_bin : string;
    kind2_module : kind2_module list;
    xml: bool;
    debug_sections : string list;
    debug_log: string;
    verbosity: int }

let pp_print_copts 
    ppf 
    { timeout_wall; 
      timeout_virtual;
      smtsolver;
      smtlogic;
      z3_bin;
      cvc4_bin;
      kind2_module;
      xml;
      debug_sections;
      debug_log;
      verbosity } =

  Format.fprintf ppf 
    "@[<v>timeout_wall: %d@,\
     timeout_virtual: %d@,\
     smtsolver: %a@,\
     smtlogic: %a@,\
     z3_bin: %s@,\
     cvc4_bin: %s@,\
     enable: @[<hv>%a@]@,\
     xml: %B@,\
     debug: @[<hv>%a@]@,\
     debug-log: %s@,\
     verbosity: %d@]" 
    timeout_wall
    timeout_virtual
    pp_print_smtsolver smtsolver
    pp_print_smtlogic smtlogic
    z3_bin
    cvc4_bin
    (Lib.pp_print_list pp_print_kind2_module ",@ ") kind2_module
    xml
    (Lib.pp_print_list Format.pp_print_string ",@ ") debug_sections
    debug_log
    verbosity

let main copts files = 

  Format.printf
    "@[<v>Options:@,%a@,Files: %a@]@."
    pp_print_copts copts
    (Lib.pp_print_list Format.pp_print_string "@,") [files]



(* Help sections common to all commands *)

let copts_sect = "COMMON OPTIONS"
let help_secs = [ 
 `S copts_sect; 
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";`Noblank;
 `P "Use `$(mname) help patterns' for help on patch matching."; `Noblank;
 `P "Use `$(mname) help environment' for help on environment variables.";
 `S "BUGS"; `P "Check bug reports at http://bugs.example.org.";]


(* Options common to all commands *)

  

let copts 
    timeout_wall
    timeout_virtual 
    smtsolver
    smtlogic
    z3_bin
    cvc4_bin
    kind2_module
    xml 
    debug_sections
    debug_log
    verbosity = 

  { timeout_wall; 
    timeout_virtual;
    smtsolver;
    smtlogic;
    z3_bin;
    cvc4_bin;
    kind2_module;
    xml;
    debug_sections;
    debug_log; 
    verbosity }

let copts_t = 
  
  let docs = "COMMON OPTIONS" in

  let timeout_wall = 
    let doc = "Limit to $(docv) seconds of wallclock time, by default do not\
               impose a limit." in
    Cmdliner.Arg.(value & 
                  opt int 0 & 
                  info ["t"; "timeout_wall"] ~docv:"SECS" ~docs ~doc)
  in

  let timeout_virtual = 
    let doc = "Limit to $(docv) seconds of CPU time, by default do not impose a\
               limit." in
    Cmdliner.Arg.(value & 
                  opt int 0 & 
                  info ["timeout_virtual"] ~docv:"SECS" ~docs ~doc)
  in

  let smtsolver =
    let doc = "Use $(docv) as SMT solver, currently supported are $(i,Z3) and \
               $(i,CVC4)." in
    Cmdliner.Arg.(value & 
                  opt 
                    (enum
                       [("Z3", `Z3_SMTLIB); 
                        ("CVC4", `CVC4_SMTLIB)])
                    `Z3_SMTLIB & 
                  info ["smtsolver"] ~docv:"SMTSOLVER" ~docs ~doc)
  in
    
  let smtlogic = 
    let doc = "Use $(docv) as the SMT logic. Available options are $(i,LIA) \
               and $(i,LRA), by default detect the logic from the input." in
    Cmdliner.Arg.(value & 
                  opt
                    (enum
                       [("LIA", `LIA); 
                        ("LRA", `LRA);
                        ("detect", `detect)]) 
                    `detect & 
                  info ["smtlogic"] ~docv:"LOGIC" ~docs ~doc)
  in
  
  let z3_bin =
    let doc = "Use $(docv) as the Z3 executable, by default search for the \
               executable $(i,z3) in the path." in
    Cmdliner.Arg.(value & 
                  opt non_dir_file "z3" & 
                  info ["z3_bin"] ~docv:"FILE" ~docs ~doc)
  in

  let cvc4_bin =
    let doc = "Use $(docv) as the CVC4 executable, by default search for the \
               executable $(i,cvc4) in the path." in
    Cmdliner.Arg.(value & 
                  opt non_dir_file "cvc4" & 
                  info ["cvc4_bin"] ~docv:"FILE" ~docs ~doc)
  in

  let kind2_module =
    let doc = "Use the model checking engine $(docv). Repeat this option to \
               run more than one engine in parallel. Available options are \
               $(i,bmc) for bounded model checking, $(i,ind) for k-induction, \
               $(i,pdr) for IC3 aka PDR and $(i,invgen) for template-based \
               invariant generation." in
    Cmdliner.Arg.(value & 
                  opt_all 
                    (enum [("pdr", `PDR); 
                           ("bmc", `BMC); 
                           ("ind", `IND);
                           ("invgen", `INVGEN)])
                    [`PDR] & 
                  info ["m"; "enable"] ~docv:"MOD" ~docs ~doc)
  in

  let xml = 
    let doc = "Output in XML format." in
    Cmdliner.Arg.(value & flag & info ["xml"] ~docs ~doc)
  in

  let debug_sections = 
    let doc = "Enable debug output for section $(docv). Repeat this option \
               to output in more than section. Only effective if debug \
               output was enabled during compilation." in
    Cmdliner.Arg.(value & opt_all string [] & info ["debug"] ~docv:"SECT" ~docs ~doc)
  in

  let debug_log =
    let doc = "Write debug output to $(decv)." in
    Cmdliner.Arg.(value & opt string "" & info ["debug-log"] ~docv:"FILE" ~docs ~doc)
  in
  
  let verbosity =
    let doc = "Set verbosity of output." in
    Cmdliner.Arg.(value & 
                  opt ~vopt:1 int 0 & info ["v"] ~docv:"LEVEL" ~docs ~doc)
  in

  Cmdliner.Term.(pure
                   copts $ 
                   timeout_wall $
                   timeout_virtual $
                   smtsolver $
                   smtlogic $
                   z3_bin $ 
                   cvc4_bin $ 
                   kind2_module $
                   xml $
                   debug_sections $ 
                   debug_log $
                   verbosity )
    



let files = Cmdliner.Arg.(value & (pos 0 non_dir_file "") & info [] ~docv:"FILE")

let cmd = 
  let doc = "an SMT-based multi-engine model checker" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) prints the last lines of each $(i,FILE) to standard output. If
        no file is specified reads standard input. The number of printed
        lines can be  specified with the $(b,-n) option." ]
  in

  (* Lift function *)
  Cmdliner.Term.(pure main $ copts_t $ files),

  (* Program name and version *)
  Cmdliner.Term.info "kind2" ~version:"0.1.2" ~doc ~man



(* Evaluate term with command line argument *)
let () = match Cmdliner.Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0

      
(* 
   Local Variables:
   compile-command: "ocamlbuild -use-ocamlfind test_kind2.native "
   indent-tabs-mode: nil
   End: 
*)
  
