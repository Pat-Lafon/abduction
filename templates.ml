open Zutils
open Prop
open Sugar
open Zdatatype

(* The feature-template layer turns user-supplied template propositions into the
   concrete feature list that the CEGIS engine ([Cegis]/[Fvec_tab]/[Feature], the
   domain-agnostic modules in this library) then searches over. The orchestrator
   installs templates via [init_template]; [Infer_prop] reads them back via
   [get_template] and expands them with [mk_features]. *)

type template = { bvars : (Nt.t, string) typed list; body : Nt.t prop }

(* A template body is a single literal or a conjunction of literals. The
   conjunction case is the one additional shape we accept beyond an atom: a
   recognizer guarding an accessor (e.g. [is_rbtnode v && color v == false]),
   which must stay one feature (see [Feature.feature_tab]). Anything else
   (implication, disjunction, nested quantifier) is rejected loudly. *)
let rec destruct_univerial_prop = function
  | Forall { qv; body } ->
      let qvs, body = destruct_univerial_prop body in
      (qv :: qvs, body)
  | Lit _ as p -> ([], p)
  | And ps as p when List.for_all (function Lit _ -> true | _ -> false) ps ->
      ([], p)
  | _ ->
      _failatwith [%here]
        "abduction template body must be a literal or a conjunction of literals \
         (e.g. a recognizer guarding an accessor)"

let prop_to_template prop =
  let fvs = fv_prop prop in
  let () =
    if List.length fvs > 0 then _failatwith [%here] "die" else ()
  in
  let bvars, body = destruct_univerial_prop prop in
  { bvars; body }

let instantiate_template vars { bvars; body } =
  let vars_list =
    List.map (fun bvar -> List.filter (fun y -> Nt.equal_nt bvar.ty y.ty) vars) bvars
  in
  let args_settings = List.choose_list_list vars_list in
  let args_settings = List.map (fun a -> List.combine bvars a) args_settings in
  let features =
    List.map
      (fun args_setting ->
        List.fold_right
          (fun (x, y) -> subst_prop_instance x.x (AVar y))
          args_setting body)
      args_settings
  in
  features

let mk_features templates vars =
  let name_to_avoid = (AbductionConfig.get ()).name_to_avoid in
  let vars =
    List.filter
      (fun x -> List.for_all (fun y -> not (String.equal x.x y)) name_to_avoid)
      vars
  in
  let features =
    List.concat @@ List.map (instantiate_template vars) templates
  in
  let features =
    features
    @ List.filter_map
        (fun x ->
          if Nt.equal_nt x.ty Nt.bool_ty then Some (Lit ((AVar x) #: Nt.bool_ty))
          else None)
        vars
  in
  let () =
    ZUtilsConfig._log_queries @@ fun _ ->
    Pp.printf "@{<bold>@{<orange>Features:@}@} %s\n"
      (List.split_by_comma layout_prop features)
  in
  features

let templates : template list option ref = ref None

(* Loads the configured abduction templates from [cfg.templates_file]: parses the file,
   keeps its [let[@axiom]] decls as the (name, prop) candidate pool, then selects the
   configured [templates] names from it. An unresolved name is a misconfig and fails
   loudly, naming the file it was expected in. *)
let init_template () =
  let cfg = AbductionConfig.get () in
  let available =
    List.filter_map
      (function
        | Language.MAxiom { name; prop; _ } -> Some (name, prop) | _ -> None)
      (Preprocess.preprocess [ cfg.templates_file ])
  in
  let props =
    List.map
      (fun name ->
        match List.assoc_opt name available with
        | Some prop -> prop
        | None ->
            _failatwith [%here]
              (Printf.sprintf "abduction template %s not found in %s" name
                 cfg.templates_file))
      cfg.templates
  in
  templates := Some (List.map prop_to_template props)

let get_template () =
  match !templates with
  | None -> _failatwith [%here] "abduction templates not initialized"
  | Some ts -> ts
