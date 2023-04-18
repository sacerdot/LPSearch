open Timed
open Cmdliner

let find_sym ~prt:_prt ~prv:_prv _sig_state {Common.Pos.elt=(mp,name) ; pos} =
 Core.Term.create_sym mp Core.Term.Public Core.Term.Defin Core.Term.Sequen false
  (Common.Pos.make pos name) Core.Term.mk_Type [] 

let answer_query {Common.Pos.elt=cmd ; _} =
 match cmd with
    Parsing.Syntax.P_query {elt=Parsing.Syntax.P_query_infer (pterm,_) ; _} ->
      let sig_state = Core.Sig_state.dummy in
      let env = [] in
      let query = Parsing.Scope.scope_lhs ~find_sym false sig_state env pterm in
      Format.printf "Query %a\n" Core.Print.term query ;
      let vs = LPSearch.Indexing.DB.search query in
      List.iter
       (fun ((p,n),pos) -> Format.printf "Equivalent to %a.%s@%a\n" Core.Print.path p n Common.Pos.pp pos)
       vs ;
      Format.printf "\n"
  | _ ->
      prerr_endline "Syntax error"

let rec search () =
  Format.printf "Enter query with syntax \"type query;\": @." ;
  match input_line stdin with
     s ->
      let aststream = Parsing.Parser.Lp.parse_string "LPSearch" s in
      Stream.iter answer_query aststream ;
      search ()
   | exception End_of_file -> ()

let index file =
 let sign = Handle.Compile.PureUpToSign.compile_file file in
 let syms = sign.sign_symbols in
 (*
 let rules = sign.sign_deps in
  Path.Map.fold
   Str.Map.fold
    List.fold
     rule ->

       sym_path sym_name
       sym_type : term ref
       sym_def : term option ref
       sym_rules : rule list ref *)
  Lplib.Extra.StrMap.iter
   (fun _ sym ->
     LPSearch.Indexing.DB.insert !(sym.Core.Term.sym_type)
      ((LPSearch.Indexing.name_of_sym sym),sym.sym_pos))
   !syms

let index_cmd files =
 Common.Library.set_lib_root (Some (Sys.getcwd ())) ;
 Stdlib.(Handle.Compile.gen_obj := true) ;
 List.iter index files ;
 search ()

let man_pkg_file =
  let sample_pkg_file =
    let lines =
      [ "# Lines whose first non-whitespace charater is # are comments"
      ; "# The end of a non-comment line cannot be commented."
      ; "# The following two fields must be defined:"
      ; "package_name = my_package_name"
      ; "root_path = a.b.c"
      ; "# Unknown fields like the following are ignored."
      ; "unknown = this is useless" ]
    in
    `Pre (String.concat "\n" (List.map (Printf.sprintf "\t%s") lines))
  in
  [ `S Manpage.s_files
  ; `P "A package configuration files $(b,LPSearch.pkg) can be placed at the \
        root of a source tree, so that LPSearch can determine under what \
        module path the underlying modules should be registered (relative to \
        the library root). If several candidate package configuration files \
        are found in the parent folders of a source file, the one in the \
        closest parent directory is used."
  ; `P "The syntax of package configuration files is line-based. Each line \
        can either be a comment (i.e., it starts with a '#') or a key-value \
        association of the form \"key = value\". Two such entries should be \
        given for a configuration file to be valid: a $(b,package_name) \
        entry whose value is an identifier and a $(b,root_path) entry whose \
        value is a module path."
  ; `P "An example of package configuration file is given bellow."
  ; sample_pkg_file ]

let files : string list Cmdliner.Term.t =
  let doc =
    Printf.sprintf
      "Source file with the [%s] extension when using the LambdaPi syntax (or with the [%s] extension when \
       using the Dedukti syntax)." Common.Library.lp_src_extension Common.Library.dk_src_extension
  in
  Arg.(value & (pos_all non_dir_file []) & info [] ~docv:"FILE" ~doc)

let index_cmd =
 let doc = "Index the given files." in
 Cmd.v (Cmd.info "index" ~doc ~man:man_pkg_file)
  Cmdliner.Term.(const index_cmd $ files)

let version = "0.1"

let _ =
 let t0 = Sys.time () in
 Stdlib.at_exit (Common.Debug.print_time t0);
 Printexc.record_backtrace true;
 let cmds = [ index_cmd ] in
 let doc = "Indexer and search for LambdaPi/Dedukti" in
 let sdocs = Manpage.s_common_options in
 let info = Cmd.info "LPSearch" ~version ~doc ~sdocs in
 let default = Cmdliner.Term.(ret (const (`Help (`Pager, None)))) in
 exit (Cmd.eval (Cmd.group info ~default cmds))
