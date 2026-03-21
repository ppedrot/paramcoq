(**************************************************************************)
(*                                                                        *)
(*     ParamCoq                                                           *)
(*     Copyright (C) 2012 - 2018                                          *)
(*                                                                        *)
(*     See the AUTHORS file for the list of contributors                  *)
(*                                                                        *)
(*   This file is distributed under the terms of the MIT License          *)
(*                                                                        *)
(**************************************************************************)


open Names
open Globnames
open Libobject

let (set_parametricity_tactic, get_parametricity_tactic, print_parametricity_tactic) = 
    Tactic_option.declare_tactic_option "Parametricity tactic"

module IntMap = Map.Make(Int)
module GMap = GlobRef.Map

let initial_translations = GMap.empty
let initial_relations = IntMap.empty

let bases = Summary.ref initial_relations ~name:"parametricity-bases"
let relations = Summary.ref initial_relations ~name:"parametricity"

let print_relations () = 
  IntMap.iter (fun n translations -> 
   GMap.iter (fun gref c -> Feedback.(msg_info (Printer.pr_global gref))) translations
  ) !relations

let add (n : int) f = 
  let translations =
    try IntMap.find n !relations with Not_found -> initial_translations
  in
  relations := IntMap.add n (f translations) !relations

let cache_relation (n, x, x_R) =
  add n (GMap.add x x_R)

let discharge_relation (n, x, x_R) =
  Some (n, x, x_R)

let subst_relation (subst, (n, x, x_R)) = 
    (n, subst_global_reference subst x, subst_global_reference subst x_R)

let in_relation = declare_object {(default_object "PARAMETRICITY") with 
                   cache_function = cache_relation;
                   load_function = (fun _ -> cache_relation);
                   subst_function = subst_relation;
                   classify_function = (fun obj -> Substitute);
                   discharge_function = discharge_relation}
 
let declare_relation n x x_R = 
 Lib.add_leaf (in_relation (n, x, x_R))

let cache_base (gr, grs) =
  let arity = Array.length grs in
  let old = match IntMap.find_opt arity !bases with
  | None -> GMap.empty
  | Some old -> old
  in
  let map = GMap.add gr grs old in
  bases := IntMap.add arity map !bases

let subst_base (subst, (gr, grs)) =
  let gr = subst_global_reference subst gr in
  let grs = CArray.Smart.map (fun gr -> Option.map (fun gr -> subst_global_reference subst gr) gr) grs in
  (gr, grs)

let discharge_base (gr, grs) =
  (* FIXME: not correct actually *)
  Some (gr, grs)

let in_base = declare_object {(default_object "PARAMETRICITY-BASE") with 
  cache_function = cache_base;
  load_function = (fun _ -> cache_base);
  subst_function = subst_base;
  classify_function = (fun obj -> Substitute);
  discharge_function = discharge_base
}

let declare_heterogeneous gr grs =
  let map grn = if GlobRef.UserOrd.equal gr grn then None else Some grn in
  let grs = List.map map grs in
  let grs = Array.of_list grs in
  Lib.add_leaf (in_base (gr, grs))

let get_heterogeneous gr ~arity ~pos =
  let map = IntMap.find arity !bases in
  match (GMap.find gr map).(pos) with
  | None -> raise Not_found
  | Some gr -> gr

let declare_constant_relation (n : int) (c : Constant.t) (c_R : Constant.t) =
  declare_relation n (GlobRef.ConstRef c) (GlobRef.ConstRef c_R)

let declare_inductive_relation (n : int) (i : inductive) (i_R : inductive) = 
  declare_relation n (GlobRef.IndRef i) (GlobRef.IndRef i_R)

let declare_variable_relation (n : int) (v : variable) (v_R : Constant.t) =
  declare_relation n (GlobRef.VarRef v) (GlobRef.ConstRef v_R)

let get_constant n c = 
  let map = IntMap.find n !relations in
  GMap.find (GlobRef.ConstRef c) map

let get_inductive n i = 
  let map = IntMap.find n !relations in
  GMap.find (GlobRef.IndRef i) map

let get_variable n v = 
  let map = IntMap.find n !relations in
  destConstRef (GMap.find (GlobRef.VarRef v) map)
  
let is_referenced n ref = 
  try
    let map = IntMap.find n !relations in 
    GMap.mem ref map
  with Not_found -> false
