open Language
open Zutils
open Sugar
open Auxtyping
open Typing
open Common
open Termcheck

type t = Nt.t

let layout_ty = Nt.layout

(* The abduced coverage type is produced at the single abduction leaf in
   [partial_term_type_infer] and threaded back up as the second component of the
   result. [None] means no leaf fired (a bare-value body needing no abduction). *)
let rec partial_value_type_infer (uctx : uctx) (a : (t, t value) typed)
    (rty : t rty) :
    ((t rty, t rty value) typed * t rty option) option =
  let res =
    match (a.x, rty) with
    | VLam { lamarg; body }, RtyArr { argrty = RtyBase { ou = Over; cty = argcty }; arg; retty } ->
        let body =
          body #-> (subst_term_instance lamarg.x (VVar arg #: lamarg.ty))
        in
        let argrty = RtyBase { ou = Over; cty = argcty } in
        let* body, inferred =
          partial_term_type_infer (add_to_rights uctx [ arg #: argrty ]) body retty
        in
        let lamarg = arg #: argrty in
        let rty = RtyArr { argrty; arg; retty = body.ty } in
        Some ((VLam { lamarg; body }) #: rty, inferred)
    | VLam { lamarg; body }, RtyArr { argrty = RtyArr _ as argrty; arg; retty } ->
        let* body, inferred =
          partial_term_type_infer
            (add_to_rights uctx [ lamarg.x #: argrty ])
            body retty
        in
        let lamarg = lamarg.x #: argrty in
        let rty = RtyArr { argrty; arg; retty = body.ty } in
        Some ((VLam { lamarg; body }) #: rty, inferred)
    | VLam _, _ -> _failatwith [%here] ""
    | VFix { fixname; fixarg; body }, RtyArr { argrty = RtyBase { ou = Over; cty = argcty }; arg; retty } ->
        let _, ret_nty = Nt.destruct_arr_tp fixname.ty in
        if String.equal "stlc_term" (layout_ty ret_nty) then
          match (body.x, retty) with
          | ( CVal { x = VLam { lamarg; body }; _ },
              RtyArr { argrty = RtyBase { ou = Over; cty = argcty1 }; arg = arg1; retty } ) ->
              let rty' =
                let arg' = { x = Rename.unique arg; ty = fixarg.ty } in
                let arg = arg #: fixarg.ty in
                let arg1' = { x = Rename.unique arg1; ty = lamarg.ty } in
                let arg1 = arg1 #: lamarg.ty in
                let rec_constraint_cty = apply_rec_arg2 arg arg' arg1 in
                RtyArr
                  {
                    argrty = RtyBase { ou = Over; cty = argcty };
                    arg = arg'.x;
                    retty =
                      RtyArr
                        {
                          argrty =
                            RtyBase
                              {
                                ou = Over;
                                cty =
                                  intersect_ctys [ argcty1; rec_constraint_cty ];
                              };
                          arg = arg1'.x;
                          retty =
                            subst_rty_instance arg1.x (AVar arg1')
                            @@ subst_rty_instance arg.x (AVar arg') retty;
                        };
                  }
              in
              let binding = arg #: (RtyBase { ou = Over; cty = argcty }) in
              let binding1 = arg1 #: (RtyBase { ou = Over; cty = argcty1 }) in
              let body =
                body
                #-> (subst_term_instance fixarg.x (VVar arg #: fixarg.ty))
                #-> (subst_term_instance lamarg.x (VVar arg1 #: fixarg.ty))
              in
              let* body', inferred =
                partial_term_type_infer
                  (add_to_rights uctx [ binding; binding1; fixname.x #: rty' ])
                  body retty
              in
              let lam =
                (VLam { lamarg = binding1; body = body' })
                #: (RtyArr
                      {
                        argrty = RtyBase { ou = Over; cty = argcty1 };
                        arg = arg1;
                        retty;
                      })
              in
              let clam = (CVal lam) #: lam.ty in
              Some
                ( (VFix
                     { fixname = fixname.x #: rty; fixarg = binding; body = clam })
                  #: rty,
                  inferred )
          | _ -> _failatwith [%here] "die"
        else
          let rec_constraint_cty = apply_rec_arg1 arg #: fixarg.ty in
          let rty' =
            let a = { x = Rename.unique arg; ty = fixarg.ty } in
            RtyArr
              {
                argrty =
                  RtyBase
                    {
                      ou = Over;
                      cty = intersect_ctys [ argcty; rec_constraint_cty ];
                    };
                arg = a.x;
                retty = subst_rty_instance arg (AVar a) retty;
              }
          in
          let binding = arg #: (RtyBase { ou = Over; cty = argcty }) in
          let body =
            body #-> (subst_term_instance fixarg.x (VVar arg #: fixarg.ty))
          in
          let* body', inferred =
            partial_term_type_infer
              (add_to_rights uctx [ binding; fixname.x #: rty' ])
              body retty
          in
          let rty =
            RtyArr
              { argrty = RtyBase { ou = Over; cty = argcty }; arg; retty = body'.ty }
          in
          Some
            ( (VFix { fixname = fixname.x #: rty; fixarg = binding; body = body' })
              #: rty,
              inferred )
    | _ -> Option.map (fun v -> (v, None)) (value_type_infer uctx a)
  in
  let () =
    match res with
    | Some (res, _) -> pprint_typing_infer_value_after uctx.rctx (a, Some res)
    | None -> ()
  in
  res

and partial_term_type_infer (uctx : uctx) (a : (t, t term) typed) (rty : t rty)
    : ((t rty, t rty term) typed * t rty option) option =
  match a.x with
  | CVal v ->
      let* v, inferred = partial_value_type_infer uctx v rty in
      Some ((CVal v) #: v.ty, inferred)
  | _ ->
      let* a = term_type_infer uctx a in
      let inferred_rty = Infer_prop.abductive_infer_rty uctx.rctx a.ty rty in
      Some (a, Some inferred_rty)
