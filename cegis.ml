open Zutils
open Prop
open Sugar
open Feature

let force_get_check_res verifier candidate () =
  match !candidate with
  | c, None ->
      let res = verifier c in
      candidate := (c, Some res);
      res
  | _, Some res -> res

let cegis features verifier sanity_check =
  let layout_vec_id fv = layout_prop @@ feature_id_to_prop features fv in
  let fvec_tab = Fvec_tab.init features sanity_check in
  let candidate = ref (mk_false, None) in
  let get_res = force_get_check_res verifier candidate in
  let get_candidate () = fst !candidate in
  let counter = ref 0 in
  let rec loop () =
    let () = counter := !counter + 1 in
    match Fvec_tab.pick_unknown fvec_tab with
    | Some fv ->
        let () =
          ZUtilsConfig._log_queries @@ fun _ ->
          Pp.printf "@{<bold>@{<orange>\tpick_maybepos: %s@}@}\n"
            (layout_vec_id fv)
        in
        let () = Fvec_tab.update fvec_tab fv Neg in
        let candidate' = Fvec_tab.get_prop_by_filter is_not_neg fvec_tab in
        if verifier candidate' then (
          let () =
            ZUtilsConfig._log_queries @@ fun _ ->
            Pp.printf "@{<bold>@{<orange>\tlabel %s as - @}@}\n"
              (layout_vec_id fv)
          in
          candidate := (Fvec_tab.get_prop_by_filter is_positive fvec_tab, None);
          loop ())
        else
          let () =
            ZUtilsConfig._log_queries @@ fun _ ->
            Pp.printf "@{<bold>@{<orange>\tlabel %s as + @}@}\n"
              (layout_vec_id fv)
          in
          let () = Fvec_tab.update fvec_tab fv Pos in
          let () =
            candidate := (Fvec_tab.get_prop_by_filter is_positive fvec_tab, None)
          in
          if get_res () then Some (get_candidate ()) else loop ()
    | None ->
        if get_res () then Some (get_candidate ())
        else _failatwith [%here] "die"
  in
  let res = if get_res () then Some (get_candidate ()) else loop () in
  res
