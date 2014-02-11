
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

module C = Cmdliner 

(* Implementations, just print the args. *)

type verb = Normal | Quiet | Verbose
type copts = { debug : bool; verb : verb; prehook : string option }

let str = Printf.sprintf 
let opt_str sv = function None -> "None" | Some v -> str "Some(%s)" (sv v)
let opt_str_str = opt_str (fun s -> s)
let verb_str = function 
  | Normal -> "normal" | Quiet -> "quiet" | Verbose -> "verbose"

let pr_copts oc copts = Printf.fprintf oc 
    "debug = %b\nverbosity = %s\nprehook = %s\n" 
    copts.debug (verb_str copts.verb) (opt_str_str copts.prehook)

let initialize copts repodir = Printf.printf
    "%arepodir = %s\n" pr_copts copts repodir

let record copts name email all ask_deps files = Printf.printf
    "%aname = %s\nemail = %s\nall = %b\nask-deps = %b\nfiles = %s\n" 
    pr_copts copts (opt_str_str name) (opt_str_str email) all ask_deps 
    (String.concat ", " files)

let help copts man_format cmds topic = match topic with
| None -> `Help (`Pager, None) (* help about the program. *)
| Some topic -> 
    let topics = "topics" :: "patterns" :: "environment" :: cmds in 
    let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
    match conv topic with 
    | `Error e -> `Error (false, e)
    | `Ok t when t = "topics" -> List.iter print_endline topics; `Ok ()
    | `Ok t when List.mem t cmds -> `Help (man_format, Some t)
    | `Ok t -> 
        let page = (topic, 7, "", "", ""), [`S topic; `P "Say something";] in
        `Ok (Cmdliner.Manpage.print man_format Format.std_formatter page)

open Cmdliner;;

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

let copts debug verb prehook = { debug; verb; prehook }
let copts_t = 
  let docs = copts_sect in 
  let debug = 
    let doc = "Give only debug output." in
    C.Arg.(value & flag & info ["debug"] ~docs ~doc)
  in
  let verb =
    let doc = "Suppress informational output." in 
    let quiet = Quiet, C.Arg.info ["q"; "quiet"] ~docs ~doc in
    let doc = "Give verbose output." in
    let verbose = Verbose, C.Arg.info ["v"; "verbose"] ~docs ~doc in 
    C.Arg.(last & vflag_all [Normal] [quiet; verbose]) 
  in 
  let prehook = 
    let doc = "Specify command to run before this $(mname) command." in 
    C.Arg.(value & opt (some string) None & info ["prehook"] ~docs ~doc)
  in
  C.Term.(pure copts $ debug $ verb $ prehook)
    
(* Commands *)

let modelcheck_cmd =
  let doc = "Run the model checker" in
  let man = [
    `S "DESCRIPTION";
    `P "Run the model checker on the given input file"] @ help_secs
  in
  C.Term.(pure modelcheck $ copts_t),
  C.Term.info "modelcheck" ~sdocs:copts_sect ~doc ~man



let initialize_cmd = 
  let repodir = 
    let doc = "Run the program in repository directory $(docv)." in
    C.Arg.(value & opt file Filename.current_dir_name & info ["repodir"]
           ~docv:"DIR" ~doc)
  in
  let doc = "make the current directory a repository" in
  let man = [
    `S "DESCRIPTION";
    `P "Turns the current directory into a Darcs repository. Any

       existing files and subdirectories become ..."] @ help_secs
  in
  C.Term.(pure initialize $ copts_t $ repodir),
  C.Term.info "initialize" ~sdocs:copts_sect ~doc ~man

let record_cmd =
  let pname = 
    let doc = "Name of the patch." in
    C.Arg.(value & opt (some string) None & info ["m"; "patch-name"] ~docv:"NAME" 
           ~doc)
  in
  let author = 
    let doc = "Specifies the author's identity." in
    C.Arg.(value & opt (some string) None & info ["A"; "author"] ~docv:"EMAIL"
           ~doc)
  in
  let all = 
    let doc = "Answer yes to all patches." in  
    C.Arg.(value & flag & info ["a"; "all"] ~doc)
  in
  let ask_deps = 
    let doc = "Ask for extra dependencies." in 
    C.Arg.(value & flag & info ["ask-deps"] ~doc)
  in
  let files = C.Arg.(value & (pos_all file) [] & info [] ~docv:"FILE or DIR") in
  let doc = "create a patch from unrecorded changes" in 
  let man = 
    [`S "DESCRIPTION";
     `P "Creates a patch from changes in the working tree. If you specify 

            a set of files ..."] @ help_secs
  in    
  C.Term.(pure record $ copts_t $ pname $ author $ all $ ask_deps $ files),
  C.Term.info "record" ~doc ~sdocs:copts_sect ~man

let help_cmd = 
  let topic = 
    let doc = "The topic to get help on. `topics' lists the topics." in 
    C.Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "display help about darcs and darcs commands" in
  let man = 
    [`S "DESCRIPTION";
     `P "Prints help about kind2 commands and options"] @ help_secs
  in
  C.Term.(ret (pure help $ copts_t $ C.Term.man_format $ C.Term.choice_names $topic)),
  C.Term.info "help" ~doc ~man

let default_cmd = 
  let doc = "an SMT-based multi-engine model checker" in 
  let man = help_secs in
  C.Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ copts_t)),
  C.Term.info "kind2" ~version:"0.2.0" ~sdocs:copts_sect ~doc ~man
       
let cmds = [initialize_cmd; record_cmd; help_cmd; modelcheck_cmd]

let () = match C.Term.eval_choice default_cmd cmds with 
| `Error _ -> exit 1 | _ -> exit 0
