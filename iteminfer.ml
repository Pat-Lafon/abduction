open Language
open Zutils
open Zdatatype
open Typing

let _log = ZUtilsConfig._log_result

let _task_infer_info name rty =
  _log @@ fun _ ->
  Pp.printf "@{<bold>Type partial infer %s:@}\n" name;
  Pp.printf "@{<bold>partial infer against with:@} %s\n" (layout_rty rty)

let _task_infer_succ name =
  _log @@ fun _ ->
  Pp.printf "@{<bold>@{<yellow>Task %s, type infer succeeded@}@}\n" name

let _task_infer_fail name =
  _log @@ fun _ ->
  Pp.printf "@{<bold>@{<red>Task %s, type infer failed@}@}\n" name

(* [NoCoverage]: body type-checked but is a bare value, nothing to abduce.
   [Failed]: inference did not discharge. *)
type 'a task_outcome = Inferred of 'a | NoCoverage | Failed

let item_infer bctx inv_m imp_m (name, rty) =
  let imp =
    StrMap.find
      (spf "The source code of given refinement type '%s' is missing." name)
      imp_m name
  in
  let () =
    _log @@ fun _ ->
    Pp.printf "@{<bold>imp_m(%s)@}\n%s\n" name (layout_typed_term imp)
  in
  let () = Statistic.create_stat name imp in
  let () = Statistic.stat_update_rty (name, counter_rty_qt_qpred rty) in
  let invs = match StrMap.find_opt inv_m name with None -> [] | Some l -> l in
  let sol, rty = instantiate_rty_by_nty [%here] rty imp.ty in
  let invs = List.map (fun x -> x#=>(map_rty (Nt.msubst_nt sol))) invs in
  let () = _task_infer_info name rty in
  let time, res =
    clock (fun () ->
        Termsyn.partial_term_type_infer
          Termcheck.{ bctx; rctx = Common.Rctx.emp name [] invs }
          imp rty)
  in
  let () = Statistic.stat_total_time (name, time) in
  let () = Statistic.store_stat stat_file in
  (* Second component is the inferred coverage type; [None] = bare-value body. *)
  match res with
  | Some (_, Some inferred) ->
      _task_infer_succ name;
      (rty_add_to_right bctx name#:rty, Inferred inferred)
  | Some (_, None) ->
      _task_infer_succ name;
      (rty_add_to_right bctx name#:rty, NoCoverage)
  | None ->
      _task_infer_fail name;
      (bctx, Failed)

(* Abduce the missing coverage type for a program with exactly one annotated
   task. Runs partial inference over every annotated task ([struc_infer]), then
   collapses to the single inferred coverage type; any other shape is a
   misconfigured benchmark and fails loudly. Sole public entry point. *)
let infer_one bctx items =
  (* Infer each task, threading bctx so later tasks see earlier inferred types. *)
  let struc_infer bctx items =
    let bctx, imp_m = Itemcheck.mk_imp_m bctx items in
    let inv_m = Itemcheck.mk_invs items in
    let tasks = Itemcheck.mk_tasks items in
    let _, results =
      List.fold_left
        (fun (bctx, results) (name, rty) ->
          let bctx, outcome = item_infer bctx inv_m imp_m (name, rty) in
          (bctx, results @ [ (name, outcome) ]))
        (bctx, []) tasks
    in
    let () =
      _log @@ fun _ ->
      Pp.printf "@{<bold>Summary (total %i tasks):@}\n" (List.length tasks)
    in
    let () =
      match List.filter (function _, Failed -> true | _ -> false) results with
      | [] ->
          _log @@ fun _ ->
          Pp.printf "@{<bold>@{<yellow>All tasks succeeded@}@}\n"
      | failed ->
          _log @@ fun _ ->
          List.iter (fun (name, _) -> _task_infer_fail name) failed
    in
    results
  in
  Templates.init_template ();
  Prover.set_z3_rlimit (AbductionConfig.get ()).rlimit;
  match struc_infer bctx items with
  | [ (_, Inferred res) ] -> res
  | [ (name, Failed) ] ->
      failwith
        (Printf.sprintf
           "abductive inference failed for annotated function %s: \
            refinement-type inference returned None, meaning an SMT subtyping \
            check did not discharge"
           name)
  | [ (name, NoCoverage) ] ->
      failwith
        (Printf.sprintf
           "abductive inference completed but produced no coverage type: \
            annotated function %s had a bare-value body, which needs no \
            abduction"
           name)
  | [] ->
      failwith
        "abductive inference found no function annotated with `let[@assert] \
         ...`; nothing to abduce"
  | results ->
      failwith
        (Printf.sprintf
           "abductive inference expected exactly one annotated function, but \
            found %i: %s"
           (List.length results)
           (String.concat ", " (List.map fst results)))
