(**************************************************************************)
(*                                                                        *)
(*     CoqParam                                                           *)
(*     Copyright (C) 2012                                                 *)
(*                                                                        *)
(*     Chantal Keller                                                     *)
(*     Marc Lasson                                                        *)
(*                                                                        *)
(*     INRIA - École Polytechnique - ÉNS de Lyon                          *)
(*                                                                        *)
(*   This file is distributed under the terms of the GNU Lesser General   *)
(*   Public License                                                       *)
(*                                                                        *)
(**************************************************************************)

open Debug
open Parametricity
open Vars
open Term
open Constr
open Libnames
open Feedback
open String

let ongoing_translation = Summary.ref false ~name:"parametricity ongoing translation"
let ongoing_translation_opacity = Summary.ref false ~name:"parametricity ongoing translation opacity"
let check_nothing_ongoing () =
  if !ongoing_translation then
    error (Pp.str "Some terms are being translated, please prove pending obligations before starting a new one. End them with the command 'Parametricity Done'.")

let obligation_message () =
  let open Pp in
  msg_notice (str "The parametricity tactic generated generated proof obligations. "
          ++  str "Please prove them and end your proof with 'Parametricity Done'. ")

let default_continuation = ignore

let parametricity_close_proof () =
  let proof_obj, terminator = Proof_global.close_proof ~keep_body_ucst_separate:false (fun x -> x) in
  let opacity = if !ongoing_translation_opacity then Vernacexpr.Opaque None else  Vernacexpr.Transparent in
  Pfedit.delete_current_proof ();
  ongoing_translation := false;
  Proof_global.apply_terminator terminator (Proof_global.Proved (opacity,None,proof_obj))

let add_definition ~opaque ~hook ~kind ~tactic name env evd term typ =
  debug Debug.all "add_definition, term = " env evd (snd (term ( evd)));
  debug Debug.all "add_definition, typ  = " env evd typ;
  debug_evar_map Debug.all "add_definition, evd  = " evd;
  let init_tac =
    let open Proofview in
    let unsafe = true in
    tclTHEN (Refine.refine ~unsafe { Sigma.run = fun sigma -> let evm = Sigma.to_evar_map sigma in (Sigma.here (snd (term evm)) sigma)}) tactic
  in
  let open Proof_global in
  let open Vernacexpr in
  ongoing_translation_opacity := opaque;
  Lemmas.start_proof name ~init_tac kind evd typ hook;
  let proof = Proof_global.give_me_the_proof () in
  let is_done = Proof.is_done proof in
  if is_done then
    parametricity_close_proof ()
  else begin
    ongoing_translation := true;
    obligation_message ()
  end

let declare_abstraction ?(opaque = false) ?(continuation = default_continuation) ?kind arity evdr env a name =
  Debug.debug_evar_map Debug.all "declare_abstraction, evd  = " !evdr;
  let program_mode_before = Flags.is_program_mode () in
  Obligations.set_program_mode !Parametricity.program_mode;
  debug [`Abstraction] "declare_abstraction, a =" env !evdr a;
  let b = Retyping.get_type_of env !evdr a in
  debug [`Abstraction] "declare_abstraction, b =" env !evdr b;
  let b = Retyping.get_type_of env !evdr a in
  let b_R = relation arity evdr env b in
  let sub = range (fun k -> prime arity k a) arity in
  let b_R = substl sub b_R in
  let a_R = fun evd ->
    let evdr = ref evd in
    let a_R = translate arity evdr env a in
    debug [`Abstraction] "a_R = " env !evdr a_R;
    debug_evar_map Debug.all "abstraction, evar_map =" !evdr;
    !evdr, a_R
  in
  let evd = !evdr in
  let hook =
    match kind_of_term a with
      | Const cte when
          (try ignore (Relations.get_constant arity (Univ.out_punivs cte)); false with Not_found -> true)
        ->
         (Lemmas.mk_hook (fun _ dcl ->
           if !ongoing_translation then error (Pp.str "Please use the 'Debug.Done' command to end proof obligations generated by the parametricity tactic.");
           Pp.(Flags.if_verbose msg_info (str (Printf.sprintf "'%s' is now a registered translation." (Names.Id.to_string name))));
            Relations.declare_relation arity (Globnames.ConstRef (Univ.out_punivs cte)) dcl;
            continuation ()))
      | _ -> (Lemmas.mk_hook (fun _ dcl -> continuation ()))
  in
  let kind = match kind with
                 None -> Decl_kinds.Global, true, Decl_kinds.DefinitionBody Decl_kinds.Definition
           | Some kind -> kind in
  let tactic = snd (Relations.get_parametricity_tactic ()) in
  add_definition ~tactic ~opaque ~kind ~hook name env evd a_R b_R;
  Obligations.set_program_mode program_mode_before


let declare_inductive name ?(continuation = default_continuation) arity evd env (((mut_ind, _) as ind, inst)) =
  let mut_body, _ = Inductive.lookup_mind_specif env ind in
  debug_string [`Inductive] "Translating mind body ...";
  let translation_entry = Parametricity.translate_mind_body name arity evd env mut_ind mut_body inst in
  debug_string [`Inductive] ("Translating mind body ... done.");
  debug_evar_map [`Inductive] "evar_map inductive " !evd;
  let size = Declarations.(Array.length mut_body.mind_packets) in
  let mut_ind_R = Command.declare_mutual_inductive_with_eliminations translation_entry [] in
  for k = 0 to size-1 do
    Relations.declare_inductive_relation arity (mut_ind, k) (mut_ind_R [], k)
  done;
  continuation ()

let translate_inductive_command arity c name =
  let (sigma, env) = Lemmas.get_current_context () in
  let (sigma, c) = Constrintern.interp_open_constr env sigma c in
  let (ind, _) as pind, _ =
    try
      Inductive.find_rectype env c
    with Not_found ->
      error (Pp.(str "Unable to locate an inductive in " ++ Printer.pr_constr_env env sigma c))
  in
  try
    let ind_R = Globnames.destIndRef (Relations.get_inductive arity ind) in
    error (Pp.(str "The inductive " ++ Printer.pr_inductive env ind ++ str " already as the following registered translation " ++ Printer.pr_inductive env ind_R))
  with Not_found ->
  let evd = ref sigma in
  declare_inductive name arity evd env pind

let declare_realizer ?(continuation = default_continuation) ?kind ?real arity evd env name (var : constr)  =
  let gref = Term.(match kind_of_term var with
     | Var id -> Globnames.VarRef id
     | Const (cst, _) -> Globnames.ConstRef cst
     | _ -> error (Pp.str "Realizer works only for variables and constants.")) in
  let typ = Typing.e_type_of env evd var in
  let typ_R = Parametricity.relation arity evd env typ in
  let sub = range (fun _ -> var) arity in
  let typ_R = Vars.substl sub typ_R in
  let cpt = ref 0 in
  let real =
    incr cpt;
    match real with Some real -> fun sigma ->
      let (sigma, term) = real sigma in
      let realtyp = Retyping.get_type_of env sigma term in
      debug [`Realizer] (Printf.sprintf "real in realdef (%d) =" !cpt) env sigma term;
      debug [`Realizer] (Printf.sprintf "realtyp in realdef (%d) =" !cpt) env sigma realtyp;
      let evdr = ref sigma in
      ignore (Evarconv.e_cumul env evdr realtyp typ_R);
      let nf, _ = Evarutil.e_nf_evars_and_universes evdr in
      let term = nf term in
      debug [`Realizer] (Printf.sprintf "real in realdef (%d), after =" !cpt) env !evdr term;
      debug [`Realizer] (Printf.sprintf "realtyp in realdef (%d), after =" !cpt) env !evdr realtyp;
      (!evdr, term)
    | None -> fun sigma ->
      (let sigma, real = new_evar_compat env sigma typ_R in
      (sigma, real))
  in
  let kind = Decl_kinds.Global, true, Decl_kinds.DefinitionBody Decl_kinds.Definition in
  let name = match name with Some x -> x | _ ->
     let name_str =  Term.(match kind_of_term var with
     | Var id -> Names.Id.to_string id
     | Const (cst, _) -> Names.Label.to_string (Names.Constant.label cst)
     | _ -> assert false)
     in
     let name_R = translate_string arity name_str in
     Names.Id.of_string name_R
  in
  let sigma = !evd in
  debug_evar_map [`Realizer] "ear_map =" sigma;
  let hook = Lemmas.mk_hook (fun _ dcl ->
    Pp.(msg_info (str (Printf.sprintf "'%s' is now a registered translation." (Names.Id.to_string name))));
    Relations.declare_relation arity gref dcl;
    continuation ()) in
  let tactic = snd (Relations.get_parametricity_tactic ()) in
  add_definition ~tactic  ~opaque:false ~kind ~hook name env sigma real typ_R

let realizer_command arity name var real =
  let (sigma, env) = Lemmas.get_current_context () in
  let (sigma, var) = Constrintern.interp_open_constr env sigma var in
  Obligations.check_evars env sigma;
  let real = fun sigma -> Constrintern.interp_open_constr env sigma real in
  declare_realizer arity (ref sigma) env name var ~real

let rec list_continuation final f l _ = match l with [] -> final ()
   | hd::tl -> f (list_continuation final f tl) hd

let rec translate_module_command ?name arity r  =
  check_nothing_ongoing ();
  let (loc, qid) = qualid_of_reference r in
  try
    let globdir = Nametab.locate_dir qid in
    match globdir with
    | DirModule (_, (mp, _)) ->
       let mb = Global.lookup_module mp in
       declare_module ?name arity mb
    | _ -> assert false
  with Not_found -> error Pp.(str "Unknown Module " ++ pr_qualid qid)

and id_of_module_path mp =
 let open Names in
 let open ModPath in
 match mp with
   | MPdot (_, lab) -> Label.to_id lab
   | MPfile dp -> List.hd (DirPath.repr dp)
   | MPbound id -> MBId.to_id id

and declare_module ?(continuation = ignore) ?name arity mb  =
  debug_string [`Module] "--> declare_module";
  let open Declarations in
  let mp = mb.mod_mp in
  match mb.mod_expr, mb.mod_type with
  | Algebraic _, NoFunctor fields
  | FullStruct, NoFunctor fields ->
     let id = id_of_module_path mp in
     let id_R = match name with Some id -> id | None -> translate_id arity id in
     debug_string [`Module] (Printf.sprintf "start module: '%s' (translating '%s')."
       (Names.Id.to_string id_R) (Names.Id.to_string id));
     let mp_R = Global.start_module id_R in
     (* I have no idea what I'm doing here : *)
     let fs = Summary.freeze_summaries ~marshallable:`No in
     let _ = Lib.start_module None id_R mp_R fs in
     list_continuation
     (fun _ ->
       debug_string [`Module] (Printf.sprintf "end module: '%s'." (Names.Id.to_string id_R));
       ignore (Declaremods.end_module ()); continuation ())
     (fun continuation -> function
     | (lab, SFBconst cb) when (match cb.const_body with OpaqueDef _ -> false | Undef _ -> true | _ -> false) ->
       let (evd, env) = ref Evd.empty, Global.env () in
       let cst = Mod_subst.constant_of_delta_kn mb.mod_delta (Names.KerName.make2 mp lab) in
       if try ignore (Relations.get_constant arity cst); true with Not_found -> false then
         continuation ()
       else
       debug_string [`Module] (Printf.sprintf "axiom field: '%s'." (Names.Label.to_string lab));
       declare_realizer ~continuation arity evd env None (mkConst cst)

     | (lab, SFBconst cb) ->
       let opaque =
         match cb.const_body with OpaqueDef _ -> true | _ -> false
       in
       let kind = Decl_kinds.(Global, cb.const_polymorphic, DefinitionBody Definition) in
       let (evdr, env) = ref Evd.empty, Global.env () in
       let cst = Mod_subst.constant_of_delta_kn mb.mod_delta (Names.KerName.make2 mp lab) in
       if try ignore (Relations.get_constant arity cst); true with Not_found -> false then
         continuation ()
       else
       let evd, ucst =
          Evd.(with_context_set univ_rigid !evdr (Universes.fresh_constant_instance env cst))
       in
       let c = mkConstU ucst in
       evdr := evd;
       let lab_R = translate_id arity (Names.Label.to_id lab) in
       debug [`Module] "field : " env !evdr c;
       (try
        let typ = Typing.e_type_of env evdr c in
        debug [`Module] "type :" env !evdr typ
       with e -> error (Pp.str  (Printexc.to_string e)));
       debug_string [`Module] (Printf.sprintf "constant field: '%s'." (Names.Label.to_string lab));
       declare_abstraction ~opaque ~continuation ~kind arity evdr env c lab_R

     | (lab, SFBmind _) ->
       let (evdr, env) = ref Evd.empty, Global.env () in
       let mut_ind = Mod_subst.mind_of_delta_kn mb.mod_delta (Names.KerName.make2 mp lab) in
       let ind = (mut_ind, 0) in
       if try ignore (Relations.get_inductive arity ind); true with Not_found -> false then
         continuation ()
       else begin
         let evd, pind =
            Evd.(with_context_set univ_rigid !evdr (Universes.fresh_inductive_instance env ind))
         in
         evdr := evd;
         debug_string [`Module] (Printf.sprintf "inductive field: '%s'." (Names.Label.to_string lab));
	 let ind_name = Names.id_of_string
          @@ translate_string arity
          @@ Names.Label.to_string
          @@ Names.MutInd.label
          @@ mut_ind
	 in
         declare_inductive ind_name ~continuation arity evdr env pind
       end
     | (lab, SFBmodule mb') when
          match mb'.mod_type with NoFunctor _ ->
            (match mb'.mod_expr with FullStruct | Algebraic _ -> true | _ -> false)
          | _ -> false
        -> declare_module ~continuation arity mb'

     | (lab, _) ->
         Pp.(Flags.if_verbose msg_info (str (Printf.sprintf "Ignoring field '%s'." (Names.Label.to_string lab))));
          continuation ()
     ) fields ()
  | Struct _, _ -> error Pp.(str "Module " ++ (str (Names.ModPath.to_string mp))
                                 ++ str " is an interactive module.")
  | Abstract, _ -> error Pp.(str "Module " ++ (str (Names.ModPath.to_string mp))
                                 ++ str " is an abstract module.")
  | _ -> Feedback.msg_warning Pp.(str "Module " ++ (str (Names.ModPath.to_string mp))
                                 ++ str " is not a fully-instantiated module.");
         continuation ()


let command_variable ?(continuation = default_continuation) arity variable names =
  error (Pp.str "Cannot translate an axiom nor a variable. Please use the 'Parametricity Realizer' command.")

let translateFullName arity (constant : Names.constant) : string =
  let nstr =
    (translate_string arity
     @@ Names.Label.to_string
     @@ Names.Constant.label
     @@ constant)in 
  let pstr =
    (Names.ModPath.to_string
     @@ Names.modpath
     @@ Names.canonical_con
     @@ constant) in
  let plstr = Str.split (Str.regexp ("\.")) pstr in
  (String.concat "_o_" (plstr@[nstr]))


let command_constant ?(continuation = default_continuation) arity constant names =
  let poly, opaque =
    let cb = Global.lookup_constant constant in
    Declarations.(cb.const_polymorphic, match cb.const_body with Def _ -> false | _ -> true)
  in
  let name = match names with
      | None -> Names.id_of_string (translateFullName arity constant)
      | Some name -> name
  in
  let kind = Decl_kinds.(Global, poly, DefinitionBody Definition) in
  let (evd, env) = Lemmas.get_current_context () in
  let evd, pconst =
    Evd.(with_context_set univ_rigid evd (Universes.fresh_constant_instance env constant))
  in
  let constr = mkConstU pconst in
  declare_abstraction ~continuation ~opaque ~kind arity (ref evd) env constr name

let command_inductive ?(continuation = default_continuation) arity inductive names =
  let (evd, env) = Lemmas.get_current_context () in
  let evd, pind =
    Evd.(with_context_set univ_rigid evd (Universes.fresh_inductive_instance env inductive))
  in
  let name = match names with
      | None ->
             Names.id_of_string
          @@ translate_string arity
          @@ Names.Label.to_string
          @@ Names.MutInd.label
          @@ fst
	  @@ fst
	  @@ pind
      | Some name -> name
  in
  declare_inductive name ~continuation arity (ref evd) env pind


let command_constructor ?(continuation = default_continuation) arity gref names =
  let open Pp in
  error ((str "'")
        ++ (Printer.pr_global gref)
        ++ (str "' is a constructor. To generate its parametric translation, please translate its inductive first."))

let command_reference ?(continuation = default_continuation) arity gref names =
   check_nothing_ongoing ();
   let open Globnames in
   match gref with
   | VarRef variable ->
     command_variable ~continuation arity variable names
   | ConstRef constant ->
     command_constant ~continuation arity constant names
   | IndRef inductive ->
     command_inductive ~continuation arity inductive names
   | ConstructRef constructor ->
     command_constructor ~continuation arity gref names

let command_reference_recursive ?(continuation = default_continuation) arity gref =
  let open Globnames in
  let gref= Globnames.canonical_gr gref in
  let label = Names.Label.of_id (Nametab.basename_of_global gref) in
  let c = printable_constr_of_global gref in
  let (direct, graph, _) = Assumptions.traverse label c in
  let inductive_of_constructor ref =
    let open Globnames in
    let ref= Globnames.canonical_gr ref in
    if not (isConstructRef ref) then ref else
     let (ind, _) = Globnames.destConstructRef ref in
     Globnames.IndRef ind
  in
  let rec fold_sort graph visited nexts f acc =
    Refset_env.fold (fun ref ((visited, acc) as visacc) ->
          let ref_ind = inductive_of_constructor ref in
          if Refset_env.mem ref_ind visited
          || Relations.is_referenced arity ref_ind  then visacc else
          let nexts = Refmap_env.find ref graph in
          let visited = Refset_env.add ref_ind visited in
          let visited, acc = fold_sort graph visited nexts f acc in
          let acc = f ref_ind acc in
          (visited, acc)
     ) nexts (visited, acc)
  in
  let _, dep_refs = fold_sort graph Refset_env.empty direct (fun x l -> (inductive_of_constructor x)::l) [] in
  let dep_refs = List.rev dep_refs in
  (* DEBUG: *)
  let open Pp in msg_info  (str "DepRefs:");
  List.iter (fun x -> let open Pp in msg_info (Printer.pr_global x)) dep_refs;
  list_continuation continuation (fun continuation gref -> command_reference ~continuation arity gref None) dep_refs ()

let translate_command arity c name =
  if !ongoing_translation then error (Pp.str "On going translation.");
  let open Constrexpr in
  let (evd, env) = Lemmas.get_current_context () in
  let (evd, c) = Constrintern.interp_open_constr env evd c in
  let cte_option =
    match Term.kind_of_term c with Term.Const cte -> Some cte | _ -> None
  in
  let poly, opaque =
    match cte_option with
    | Some (cte, _) ->
        let cb = Global.lookup_constant cte in
        Declarations.(cb.const_polymorphic,
             match cb.const_body with Def _ -> false
                                        | _ -> true)
    | None -> false, false
  in
  let kind = Decl_kinds.(Global, poly, DefinitionBody Definition) in
  declare_abstraction ~opaque ~kind arity (ref evd) env c name
